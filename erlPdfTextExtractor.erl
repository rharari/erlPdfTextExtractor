%% @author Ricardo A. Harari - ricardo.harari@gmail.com
%% @doc pdf2txt/2 - extract txt from a pdf file and save it to disk
%% @doc ex: erlPdfTextExtractor:pdf2txt("/inbox/file1.pdf", "/outbox/file1.txt")
%% TODO: finish this
-module(erlPdfTextExtractor).

%% ====================================================================
-export([pdf2txt/2]).

pdf2txt(PdfFile, TxtOutFile) ->
	case file:read_file(PdfFile) of
		{error, Why} -> {error, Why};
		{ok, Content} -> file:write_file(TxtOutFile, process(Content))
	end.

%% ====================================================================
cut([], _, _) -> {[], 0};
cut(Content, S1, S2) ->
	L1 = byte_size(S1), L2 = byte_size(S2),
	case binary:match(Content, S1) of
		{I1, L1} -> 
			case binary:match(Content, S2) of
				{I2, L2} -> {binary:part(Content, I1 + L1, I2 - I1 - L1), I2 + L2};
				_Else -> {[], 0}
			end;
		_Else -> {[], 0}
	end.

process(Content) ->
	Lst = getLstObj(Content),
	processObj(Lst).

getLstObj(Content) ->
	case cut(Content, <<"obj">>, <<"endobj">>) of
		{_, 0} -> [];
		{Z, N} -> [Z] ++ getLstObj(binary:part(Content, N, byte_size(Content) - N))
	end.

processObj([]) -> [];
processObj([H|T]) ->
	Option = getObjOption(H),
	case shouldProcess(Option) of
		false -> processObj(T);
		true -> [decodeObj(H, Option)] ++ processObj(T)
	end.

getObjOption(O) ->
	case cut(O, <<"<<">>, <<">>">>) of
		{_, 0} -> [];
		{Z, _} -> Z
	end.

decodeObj(Obj, Option) ->
	case shouldProcess(Option) of
		false -> [];
		true -> retrieveStream(Obj, Option)
	end.

matchOption([], _) ->
	false;

matchOption(Option, What) ->
	Check = fun(X) when X =:= nomatch -> false; (_) -> true end,
	Check(binary:match(Option, What)).

shouldProcess(Option) ->	
	not (matchOption(Option, <<"Length1">>) andalso matchOption(Option, <<"Type">>) andalso matchOption(Option, <<"Subtype">>)).

retrieveStream(Obj, Option) ->
	case matchOption(Option, <<"Length">>) of 
		false -> stripStream(Obj);
		true ->
			Z = uncompress(stripStream(Obj)),
			Data = cutBTET(Z),
			case Data of 
				[] -> transform(Z);
				_ -> stripChar(Data)
			end
	end.

transform(Z) ->
	% TODO transform beginbfchar and beginbfrange   
	[].

stripStream(Content) ->
	case binary:match(Content, <<"stream">>) of
		{I0tmp, 6} ->
			I0 = I0tmp + skipChar(binary:part(Content, I0tmp + 6, 1)) + skipChar(binary:part(Content, I0tmp + 7, 1)) + 6, 
			case binary:match(Content, <<"endstream">>) of
				{I1tmp, 9} ->
					I1 = I1tmp - skipChar(binary:part(Content, I1tmp - 2, 1)) - skipChar(binary:part(Content, I1tmp - 1, 1)),
					binary:part(Content, I0, I1-I0);
				_Else -> []
			end;
		_Else -> []
	end.

stripChar(Z) ->
	case binary:match(Z, <<"(">>) of
		{I0, 1} ->
				Z1 = binary:part(Z, I0 + 2, 1),
				case Z1 of 
					<<")">> -> [binary:part(Z, I0 + 1, 1)] ++ stripChar(binary:part(Z, I0 + 3, byte_size(Z) - I0 - 3));
					_Else -> []
				end;
		_Else -> []
	end.

cutBTET([]) -> [];
cutBTET(Z) -> A1 = [X || {X, 2} <- binary:matches(Z, [<<"BT">>, <<"ET">>])], cutBTET2(Z, A1).
cutBTET2(Z, [H1,H2|T]) -> iolist_to_binary([binary:part(Z, H1 + 2, H2 - H1 - 2)] ++ [cutBTET2(Z, T)] );
cutBTET2(_, _) -> [].

uncompress(Z) ->
	try zlib:uncompress(Z) of Y -> Y
	catch error:data_error -> [] % ie: DCTDecode indicates a JPEG, etc... - discard it
	end.

skipChar(Z) when Z =:= <<14>> orelse Z =:= <<10>> -> 1; % /r or /n
skipChar(_) -> 0.

% deprecated
%zcutBTET([]) -> [];
%zcutBTET(Z) ->
%	case binary:match(Z, <<"BT">>) of
%		{I0, 2} ->
%			case binary:match(Z, <<"ET">>) of
%				{I1, 2} -> iolist_to_binary([binary:part(Z, I0 + 2, I1 - I0 - 2)] ++ cutBTET(binary:part(Z, I1 + 2, byte_size(Z) - I1 - 2)));
%				_Else -> []
%			end;
%		_Else -> []
%	end.

%% @author Ricardo A. Harari - ricardo.harari@gmail.com
%% @doc pdf2txt/2 - extract txt from a pdf file and save it to disk
%% @doc ex: erlPdfTextExtractor:pdf2txt("/inbox/file1.pdf", "/outbox/file1.txt")
%% TODO: fix some bugs 
-module(erlPdfTextExtractor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([pdf2txt/2, extract/1]).

pdf2txt(PdfFile, TxtOutFile) ->
	case extract(PdfFile) of
		{error, Why} -> {error, Why};
		Txt -> file:write_file(TxtOutFile, Txt)
	end.

extract(PdfFile) ->
	case file:read_file(PdfFile) of
		{error, Why} -> {error, Why};
		{ok, Content} -> 
			stripStream(Content)
	end.	

%% ====================================================================
%% Internal functions
%% ====================================================================
stripStream(Content) ->
	case binary:match(Content, <<"stream">>) of
		{I0tmp, 6} ->
			I0 = I0tmp + skipChar(binary:part(Content, I0tmp + 6, 1)) + skipChar(binary:part(Content, I0tmp + 7, 1)) + 6, 
			case binary:match(Content, <<"endstream">>) of
				{I1tmp, 9} ->
					I1 = I1tmp - skipChar(binary:part(Content, I1tmp - 2, 1)) - skipChar(binary:part(Content, I1tmp - 1, 1)),
					% Data = stripChar(stripBTET(uncompress(binary:part(Content, I0, I1-I0)))),
					Data = stripChar(uncompress(binary:part(Content, I0, I1-I0))),
					[Data] ++ stripStream( binary:part(Content, I1tmp + 9, byte_size(Content) - I1tmp - 9));
				_Else -> []
			end;
		_Else -> []
	end.

 % TODO: is not considering numbers and all text
stripChar([]) -> [];
stripChar(Z) ->
	case binary:match(Z, <<"(">>) of
		{I0, 1} ->
				Z1 = binary:part(Z, I0 + 2, 1), % TODO: may raise exception if I0+2 greather than Z length
				case Z1 of 
					<<")">> -> [binary:part(Z, I0 + 1, 1)] ++ stripChar(binary:part(Z, I0 + 3, byte_size(Z) - I0 - 3));
					_Else -> []
				end;
		_Else -> []
	end.


cutBTET([]) -> [];
cutBTET(Z) ->
	case binary:match(Z, <<"BT">>) of
		{I0, 2} ->
			case binary:match(Z, <<"ET">>) of
				{I1, 2} -> binary:part(Z, I0 + 2, I1 - I0 - 2);
				_Else -> []
			end;
		_Else -> []
	end.


uncompress(Z) ->
	try zlib:uncompress(Z) of Y -> Y
	catch error:data_error -> [] % ie: DCTDecode indicates a JPEG, etc... - discard it
	end.

skipChar(<<14>>) -> 1; % /r
skipChar(<<10>>) -> 1; % /n
skipChar(_) -> 0.

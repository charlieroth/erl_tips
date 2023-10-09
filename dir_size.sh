#!/usr/bin/env escript

main([StartingDir]) ->
    try
        traverse_files(StartingDir, "")
    catch
        _:_ ->
            usage()
    end;

main(_) -> usage().

traverse_files(Dir, Padding) ->
    {ok, Filenames} = file:list_dir(Dir),
    {Files, Dirs} = lists:splitwith(fun(Filename) ->
        Path = string:concat(string:concat(Dir, "/"), Filename),
        {ok, FileInfo} = file:read_file_info(Path),
        {_, _FileSize, FileType, _, _, _, _, _, _, _, _, _, _, _} = FileInfo,
        FileType =:= regular
    end, Filenames),
    TotalSizeBytes = total_size(Dir, Files, 0),
    print_size(Padding, Dir, TotalSizeBytes),
    Path = string:concat(Dir, "/"),
    traverse_dirs(Path, Padding, Dirs).

traverse_dirs(_Path, _Padding, []) -> ok;
traverse_dirs(Path, Padding, [Dir | Rest]) ->
    traverse_files(string:concat(Path, Dir), string:concat(Padding, "  ")),
    traverse_dirs(Path, Padding, Rest).
    
total_size(_Dir, [], TotalSize) -> TotalSize;
total_size(Dir, [File | Rest], TotalSize) ->
    Path = string:concat(string:concat(Dir, "/"), File),
    {ok, FileInfo} = file:read_file_info(Path),
    {_, FileSize, _, _, _, _, _, _, _, _, _, _, _, _} = FileInfo,
    total_size(Dir, Rest, TotalSize + FileSize).

bytes_to_kilobytes(Bytes) -> Bytes / 1024.
bytes_to_megabytes(Bytes) -> bytes_to_kilobytes(Bytes) / 1024.

print_size(Padding, Dir, Bytes) ->
    if
        Bytes < 1024 ->
            io:format("~s~s - ~pB~n", [Padding, Dir, Bytes]);
        Bytes < 1024 * 1024 ->
            TotalSizeKB = bytes_to_kilobytes(Bytes),
            io:format("~s~s - ~.2fKB~n", [Padding, Dir, TotalSizeKB]);
        true ->
            TotalSizeMB = bytes_to_megabytes(Bytes),
            io:format("~s~s - ~.2fMB~n", [Padding, Dir, TotalSizeMB])
    end.

usage() ->
    io:format("usage: dir_size.sh tips~n"),
    halt(1).
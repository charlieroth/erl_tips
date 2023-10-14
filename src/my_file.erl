-module(my_file).
-export([stat/1, lstat/1]).

stat(Path) ->
    {ok, FileInfo} = file:read_file_info(Path),
    print_info(FileInfo).

lstat(Path) ->
    {ok, LinkInfo} = file:read_link_info(Path),
    print_info(LinkInfo).

print_info(Info) ->
    {
        _, 
        Size, 
        Type, 
        Access, 
        Atime, 
        Mtime, 
        Ctime, 
        Mode, 
        Links, 
        MajorDevice, 
        MinorDevice, 
        INode, 
        Uid, 
        Gid
    } = Info,
    io:format("size: ~p~n", [Size]),
    io:format("type: ~p~n", [Type]),
    io:format("access: ~p~n", [Access]),
    io:format("atime: ~p~n", [Atime]),
    io:format("mtime: ~p~n", [Mtime]),
    io:format("ctime: ~p~n", [Ctime]),
    io:format("mode: ~p~n", [Mode]),
    io:format("links: ~p~n", [Links]),
    io:format("major_device: ~p~n", [MajorDevice]),
    io:format("minor_device: ~p~n", [MinorDevice]),
    io:format("inode: ~p~n", [INode]),
    io:format("uid: ~p~n", [Uid]),
    io:format("gid: ~p~n", [Gid]).

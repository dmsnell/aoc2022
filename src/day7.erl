-module(day7).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

-type path() :: binary().

-record(inode, {
    path           :: path(),
    contained_size :: non_neg_integer(),
    type           :: data | {dir, INodes :: #{path() => non_neg_integer()}}
}).


-record(system, {
    cwd       :: path(),
    fs        :: #{path() => #inode{}}
}).


input_type() -> lines.

parse_input(Lines) ->
    parse_shell_lines(Lines).

%%%
%%% Process shell sessions into commands and outputs.
%%% Walk through the lines and start with commands.
%%% When encountering a command, accumulate the lines out
%%% output, which are all lines until the next command.
%%% Once reaching the next command, or the end of the input,
%%% parse the command output, which is contextually
%%% defined by the command that created it.
%%%

parse_shell_lines(Lines) ->
    parse_shell_lines(Lines, nil, []).

%%%
%%% Parsing shell session, determing if we're
%%% issuing a new command, accumulating command
%%% output, or reaching the end of the session.
%%% 

parse_shell_lines([], {Command, CommandOutput}, Parsed) ->
    lists:reverse([parse_command_output(Command, lists:reverse(CommandOutput)) | Parsed]);

parse_shell_lines([<<"$ ", CommandInput/binary>> | Lines], nil, Parsed) ->
    Command = parse_shell_command(binary:split(CommandInput, <<" ">>, [global])),
    parse_shell_lines(Lines, {Command, []}, Parsed);

parse_shell_lines([<<"$ ", _/binary>> | _] = Lines, {Command, CommandOutput}, Parsed) ->
    parse_shell_lines(Lines, nil, [parse_command_output(Command, lists:reverse(CommandOutput)) | Parsed]);

parse_shell_lines([Line | Lines], {Command, CommandOutput}, Parsed) ->
    parse_shell_lines(Lines, {Command, [Line | CommandOutput]}, Parsed).

%%%
%%% Command syntax parsing
%%%

parse_shell_command([<<"cd">>, Path]) ->
    Destination = case Path of
        <<"..">>                 -> out;
        <<"/", _AbsPath/binary>> -> {in, absolute, Path};
        _RelativePath            -> {in, relative, Path}
    end,
    {command, cd, Destination};

parse_shell_command([<<"ls">>]) ->
    {command, ls}.


%%%
%%% Command output parsing
%%%


parse_command_output({command, cd, Destination}, []) ->
    {command, cd, Destination};

parse_command_output({command, ls}, Lines) ->
    {command, ls, lists:map(fun parse_ls_output/1, Lines)}.


parse_ls_output(<<"dir ", Name/binary>>) ->
    {dir, Name};
parse_ls_output(File) ->
    [Size, Filename] = binary:split(File, <<" ">>, [global]),
    {file, Filename, binary_to_integer(Size)}.


%%%
%%% Solving the problem
%%%


p1(Commands) ->
    #system{fs = FS} = lists:foldl(fun process_command/2, initial_system(), Commands),
    maps:fold(
        fun
            (_Path, #inode{contained_size = Size, type = {dir, _INodes}}, Total) when Size =< 100000 ->
                Total + Size;

            (_Path, _INode, Total) ->
                Total
        end,
        0,
        FS
    ).


p2(Commands) ->
    #system{fs = FS} = lists:foldl(fun process_command/2, initial_system(), Commands),
    #{<<"/">> := #inode{contained_size = UsedSpace}} = FS,
    DiskSize = 70000000,
    NeedFree = 30000000,
    NeedToFree = max(0, UsedSpace - (DiskSize - NeedFree)),
    maps:fold(
        fun
            (_Path, #inode{contained_size = Size, type = {dir, _INodes}}, Smallest)
                when Size >= NeedToFree, Size <  Smallest ->
                    Size;

            (_Path, _INode, Smallest) ->
                Smallest
        end,
        DiskSize,
        FS
    ).


%%%
%%% Semantic shell processing.
%%%


initial_system() ->
    Root = <<"/">>,
    #system{
        cwd = Root,
        fs  = #{
            Root => #inode{path = Root, contained_size = 0, type = {dir, #{}}}
        }
    }.


process_command({command, cd, Destination}, #system{cwd = CWD} = System) ->
    NewCWD = case {CWD, Destination} of
        % Cannot go above root
        {<<"/">>, out}            -> <<"/">>;
        {CWD, out}                -> parent_path(CWD);
        {_, {in, absolute, Path}} -> Path;
        {_, {in, relative, Path}} -> append_path(CWD, Path)
    end,
    System#system{cwd = NewCWD};

process_command({command, ls, Output}, System) ->
    process_ls_output(Output, System).


process_ls_output([], System) ->
    System;
process_ls_output([{file, Name, Size} | Output], #system{cwd = CWD, fs = FS} = System) ->
    Path = append_path(CWD, Name),
    FileNode = #inode{path = Path, contained_size = Size, type = data},
    SizeDelta = case maps:get(Path, FS, enoent) of
        #inode{contained_size = OldSize} -> Size - OldSize;
        enoent                           -> Size
    end,
    #inode{type = {dir, INodes}} = Dir = maps:get(CWD, FS),
    FSWithFile = FS#{Path => FileNode},
    NewFS = fs_add_size(FSWithFile, Dir#inode{type = {dir, INodes#{Path => 0}}}, SizeDelta),
    process_ls_output(Output, System#system{fs = NewFS});
process_ls_output([{dir, Name} | Output], #system{cwd = CWD, fs = FS} = System) ->
    NewDirPath = append_path(CWD, Name),
    #inode{type = {dir, INodes}} = Dir = maps:get(CWD, FS),
    FSWithDir = FS#{
        NewDirPath => #inode{path = NewDirPath, contained_size = 0, type = {dir, #{}}},
        CWD => Dir#inode{type = {dir, INodes#{NewDirPath => 0}}}
    },
    process_ls_output(Output, System#system{fs = FSWithDir}).


fs_add_size(FS, #inode{path = <<"/">>, contained_size = OldSize} = Root, Delta) ->
    FS#{<<"/">> => Root#inode{contained_size = OldSize + Delta}};

fs_add_size(FS, #inode{path = Path, contained_size = OldSize} = OldDir, Delta) ->
    Parent = maps:get(parent_path(Path), FS),
    fs_add_size(FS#{Path => OldDir#inode{contained_size = OldSize + Delta}}, Parent, Delta).


%%%
%%% Helper functions
%%% 

append_path(<<"/">>, Path) -> <<"/", Path/binary>>;
append_path(Base, Path)    -> <<Base/binary, "/", Path/binary>>.

parent_path(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, <<>>] ->
            <<"/">>;
        [<<>> | Segments] ->
            [_CWD | ReverseSegments] = lists:reverse(Segments),
            erlang:list_to_binary([<<"/">> | lists:join(<<"/">>, lists:reverse(ReverseSegments))])
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertMatch({day7, p1, 95437, _}, aoc:solve(day7, p1, "day7_a")).

p1_answer_test() ->
    ?assertMatch({day7, p1, 1644735, _}, aoc:solve(day7, p1)).


p2_test() ->
    ?assertEqual(24933642, p2(parse_input(input:lines("day7_a")))).

p2_answer_test() ->
    ?assertMatch({day7, p2, 1300850, _}, aoc:solve(day7, p2)).

-endif.
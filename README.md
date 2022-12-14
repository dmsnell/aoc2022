Advent of Code 2022 Solutions
=====

Running the solutions
-----

To run a particular solution call `aoc:solve/2` with the day
and the part specified. The day is an atom like `day1` and the
part is either `p1` for part one or `p2` for part two.

The output from each solution identifies which day and part ran,
the output for that problem, and the time it took to run.

To run all the solutions call `aoc:solve_all()`.

Start by cloning the repo and running `rebar3 shell` for a REPL.

```erlang
aoc:solve_all() = [
    {day1,p1,70764,{0.086,ms}},
    {day1,p2,203905,{0.098,ms}},
    {day2,p1,13675,{0.081,ms}},
    {day2,p2,14184,{0.089,ms}},
    {day3,p1,8515,{0.512,ms}},
    {day3,p2,2434,{0.956,ms}}
].

aoc:solve(day1, p1) = {day1,p1,70764,{0.24,ms}}.

{Day, Part, Answer, Runtime} = aoc:solve(day1, p1),
Day = day1,
Part = p1,
Answer = 70764,
Runtime = {0.24,ms}.
```

To develop with an open REPL call `r3:compile()` from within
the REPL to reload code with updates from your editor.

```erlang
% Reload code and re-run all solutions
r3:compile(), aoc:solve_all().
```

Fetching the input for each day
-----

It's possible to automatically fetch the input file for each day.
To do this you will need to store a cookie for your logged-in session.
Grab the cookie from an authenticated session in your browser and
store the contents of the cookie's value in `priv/.aoc-session.cookie`.
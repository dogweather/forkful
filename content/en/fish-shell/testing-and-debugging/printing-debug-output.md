---
date: 2024-01-20 17:52:28.158859-07:00
description: 'How to: Get cozy with `echo` - the Swiss Army knife for output in Fish.
  Here''s how to sprinkle some debug prints into your shell scripts.'
lastmod: '2024-03-13T22:45:00.476610-06:00'
model: gpt-4-1106-preview
summary: Get cozy with `echo` - the Swiss Army knife for output in Fish.
title: Printing debug output
weight: 33
---

## How to:
Get cozy with `echo` - the Swiss Army knife for output in Fish. Here's how to sprinkle some debug prints into your shell scripts.

```Fish Shell
function greet
    set name $argv[1]
    echo "Hey, $name! Let's debug."
    echo "Running the greet function" >&2
end

greet "Ada"
```
Sample output:
```
Hey, Ada! Let's debug.
Running the greet function
```
Standard out (`stdout`) is your script's main stage, but for debug chatter, use standard error (`stderr`) with `>&2`.

## Deep Dive
Back when monitors were as deep as they were wide, output was precious. Standard out (`stdout`) became the pure, user-facing channel, while standard error (`stderr`) turned into the back-alley for programmer-only gossip, like debug info.

In Fish, standard commands for output are `echo`, `printf`, and `print`. The `echo` is straightforward and mostly used for simple messages and inline debug.

You're not stuck with just `echo`, though. Prefer `printf` for formatted strings, or use redirection (`>` or `>>`) to dump debug info into a file for later.

As for implementation, using `stderr` for debug output is a convention from the Unix world, helping to separate the wheat (actual output) from the chaff (debug noise). This means users can still pipe your script's real output without getting debug garble mixed in.

## See Also
- Fish Shell Documentation on [Commands](https://fishshell.com/docs/current/commands.html)
- StackOverflow: Discussions and examples of [debugging in Fish](https://stackoverflow.com/questions/tagged/fish)
- Greg's Wiki: In-depth info on [I/O redirection](https://mywiki.wooledge.org/BashGuide/InputAndOutput#Redirection)

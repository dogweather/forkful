---
title:                "Reading command line arguments"
aliases:
- /en/fish-shell/reading-command-line-arguments/
date:                  2024-01-20T17:56:02.966338-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Reading command line arguments is grabbing the extra bits you type after your script's name, like secret handshakes to customize a script's behavior. Programmers do it to make scripts flexible and interactive without a fuss.

## How to:

Let's say `greet.fish` is your script. You want it to take a name and spit out a greeting.

```fish
#!/usr/bin/env fish

# The arguments are stored in $argv
# $argv[1] is the first argument, $argv[2] the second, and so on.

set name $argv[1]
echo "Hello, $name!"
```

Run it:

```shell
$ fish greet.fish World
Hello, World!
```

Now, with multiple arguments:

```fish
#!/usr/bin/env fish

# Loop through all arguments
for arg in $argv
    echo "Hello, $arg!"
end
```

Try it:

```shell
$ fish greet.fish Earth Mars Venus
Hello, Earth!
Hello, Mars!
Hello, Venus!
```

To handle flags (like `-u` for uppercase):

```fish
#!/usr/bin/env fish

# Check for a "-u" argument
set -l uppercase_mode off
for arg in $argv
    if test "$arg" = "-u"
        set uppercase_mode on
    else if set -q uppercase_mode[1]; and string match --quiet -- "$uppercase_mode" "on"
        echo (string upper "$arg")
    else
        echo $arg
    end
end
```

And invoke:

```shell
$ fish greet.fish -u mercury venus
MERCURY
VENUS
```

## Deep Dive

Fish Shell has had command line arguments nailed down for a long time, much like other shells. What sets Fish apart is its simplicity by design. There's no `$1, $2... $n` to remember; it's an array `$argv`, familiar territory if you dabble in other programming languages.

There are alternatives, like bash, zsh, etc., but Fish's scripting syntax aims to be more readable and straightforward. Instead of traditional `shift` commands or dealing with `$@` for all arguments, Fish has that friendly `$argv` and lovely scripting constructs like `for` loops and `if` conditions that are less about cryptic symbols and more about clear words.

When implementing, it's vital to consider how your script will be used. Will it need default values? Will users know what to input? Make sure you handle cases where users forget to pass arguments or pass them in the wrong order.

## See Also

- The official Fish documentation on command line arguments: [fishshell.com/docs/current/#syntax-command-line](https://fishshell.com/docs/current/#syntax-command-line)
- For advanced scripting and creating your own functions in Fish: [fishshell.com/docs/current/#defining-functions](https://fishshell.com/docs/current/#defining-functions)
- An introduction to Fish for users with a background in other shells: [fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)

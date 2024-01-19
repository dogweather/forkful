---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Bash 101: Reading Command Line Arguments

## What & Why?

Imagine a Bash script bowling alley, you're the bowler, and the command line arguments are the bowling balls. Each uniquely shaped and weighted ball allows you to execute shots with different curves, speeds and spins. Basically, command line arguments give you the flexibility to vary the behavior of your scripts. 

## How to:

Reading command line arguments in Bash is as simple as referencing `$1`, `$2`, etc. Here’s an example:

```Bash
#!/bin/bash

echo "First Argument: $1"
echo "Second Argument: $2"
```

If you run the script above as `./script.sh hello world`, it will output:

```Bash
First Argument: hello
Second Argument: world
```
The magical variable `$0` is your script’s name, while `$#` will tell you how many arguments were passed. `"$@"` or `"$*"` hold all of the arguments.

## Deep Dive

Historically, the use of command line arguments stems from early Unix systems where efficient computing resources were crucial and UIs were heinous or non-existent.

Alternatives to command line arguments include reading input during script execution or hardcoding values into the script. The downside? They lack the dynamism and convenience of arguments.

Implementation-wise, bash stores command line arguments as a special array. `$1` is shorthand for saying "give me the first element of this array". Bash also supports more complex array-related operations if arguments aren't your only play.

## See Also

For more thorough coverage on this topic, check out:

- [Bash Guide for Beginners](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_02.html)
- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Array Tutorial](https://www.linuxjournal.com/content/bash-arrays)
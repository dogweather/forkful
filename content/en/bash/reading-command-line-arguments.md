---
title:                "Bash recipe: Reading command line arguments"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Command line arguments are an essential part of Bash programming. They allow us to pass information or parameters to our scripts when running them in the terminal. Without understanding how to read command line arguments, we limit our coding capabilities and hinder our efficiency.

## How To

To read command line arguments in a Bash script, we use the $1, $2, $3, and so on, to indicate the position of the arguments passed. Let's take a look at an example:

```Bash
#!/bin/bash

echo "Hello $1!"
echo "My name is $2."

# Running the script with arguments "John" and "Smith" would output:
# Hello John!
# My name is Smith.
```

In the above code, we are using the $1 and $2 to read the first and second argument respectively. We can also use $0 to retrieve the name of the script itself.

We can also use the $@ or $* to read all the arguments passed. Let's see how it works in practice:

```Bash
#!/bin/bash

echo "Hello $1, $2, and $3!"

# Running the script with arguments "John", "Jane" and "Sam" would output:
# Hello John, Jane, and Sam!
```

As you can see, using $@ or $* allows us to read multiple arguments in one go.

We can also use the $# to get the count of arguments passed. Let's see how it works:

```Bash
#!/bin/bash

echo "You have passed $# arguments."

# Running the script with arguments "John", "Jane" and "Sam" would output:
# You have passed 3 arguments.
```

We can also use conditions and loops with command line arguments to make our scripts more dynamic and interactive.

## Deep Dive

To understand the concept of command line arguments better, let's take a closer look at how they work behind the scenes.

When we run a script with arguments, the command line parses them and stores them in an array called "argv". The first element of this array (argv[0]) is the name of the script, followed by the arguments in the same order as they were passed. We can access this array in our script using the $@ or $*.

Similarly, $# stores the count of the arguments, and $0 contains the name of the script.

By understanding how command line arguments are stored and accessed, we can use them effectively in our scripts and create powerful and efficient code.

## See Also

Here are some useful resources to help you learn more about command line arguments in Bash:

- [Bash Scripting Tutorial - Command Line Arguments](https://www.learnshell.org/en/Command_Line_Arguments)
- [Shell Scripting: How-to Read Command Line Arguments](https://linuxcommand.org/lc3_wss0120.php)
- [Command Line Arguments in Bash Script with Examples](https://linuxhint.com/command-line-arguments-bash/)

By mastering the use of command line arguments, we can enhance our Bash scripting skills and take our coding abilities to the next level. So, go ahead and experiment with different ways of reading command line arguments to become a more efficient developer. Happy coding!
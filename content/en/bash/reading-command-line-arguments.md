---
title:    "Bash recipe: Reading command line arguments"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you’re new to Bash programming, you may have come across the term “command line arguments” and wondered what they are and why they’re important. Simply put, command line arguments are extra pieces of information that can be passed to a script or program when it’s executed. These arguments can be used to customize the behavior of the script and make it more versatile.

## How To

To make use of command line arguments in your Bash scripts, you’ll need to follow these steps:

1. Declare the arguments you want to use by using the `$1`, `$2`, etc. variables. These variables correspond to the first, second, and so on arguments passed to the script.

2. Inside your script, you can access the values of these arguments using the `$1`, `$2`, etc. variables.

3. Enclose your script in a `getopts` loop to handle multiple arguments and options in an organized way. This allows you to specify different behaviors for different arguments and handle errors gracefully.

```Bash
#!/bin/bash

# Declaring arguments
first_argument=$1
second_argument=$2

# Accessing the values of arguments
echo "The first argument is: $first_argument"
echo "The second argument is: $second_argument"

# Handling arguments using getopts loop
while getopts "a:b:" option; do
    case $option in
        a) echo "Option A was chosen with argument: $OPTARG" ;;
        b) echo "Option B was chosen with argument: $OPTARG" ;;
        *) echo "Invalid option chosen" ;;
    esac
done
```

To execute this script, you can use the following command: `bash script.sh argument1 argument2 -a option1 -b option2`. The output will be:

```
The first argument is: argument1
The second argument is: argument2
Option A was chosen with argument: option1
Option B was chosen with argument: option2
```

## Deep Dive

There are a few important things to keep in mind when working with command line arguments. Firstly, keep in mind that the name of your script itself counts as the first argument, and the arguments passed start from the second position (designated by `$1`). Secondly, you can use the special variable `$#` to get the total number of arguments passed to your script, which can be useful for error handling. Lastly, you can use `shift` to shift the positional parameters to the left, allowing you to access arguments beyond the tenth position.

## See Also

To learn more about command line arguments and Bash scripting, check out the following resources:

- [Bash Guide for Beginners](https://linux.die.net/Bash-Beginners-Guide) - A comprehensive guide to Bash scripting, including information on command line arguments.
- [The Linux Command Line](http://linuxcommand.org/tlcl.php) - A book that covers the basics of working with the command line.
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/) - The official manual for Bash, with detailed information on command line arguments and other topics.
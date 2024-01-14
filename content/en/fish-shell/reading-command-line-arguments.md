---
title:    "Fish Shell recipe: Reading command line arguments"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming, there are often multiple ways to achieve the same task. One of the options for passing input and parameters to a script or program is through command line arguments. In this blog post, we will explore how to read command line arguments in Fish Shell.

## How To

Coding examples will be provided using the Fish Shell syntax and the resulting output will be shown. Below is a basic example of how to read command line arguments in Fish Shell:

```
Fish Shell: How to Read Command Line Arguments

#!/usr/bin/env fish

for arg in $argv
    echo "Argument: $arg"
end

```

Running the above code with the command ```fish read_args.fish arg1 arg2``` will produce the following output:

```
Argument: arg1
Argument: arg2
```

We can also use the built-in ```set``` command to assign command line arguments to variables as shown below:

```
Fish Shell: Reading Command Line Arguments Into Variables

#!/usr/bin/env fish

set argument1 $argv[1]
set argument2 $argv[2]

echo "First argument: $argument1"
echo "Second argument: $argument2"

```

Running ```fish read_args.fish this_is_arg1 this_is_arg2``` will give us the output:

```
First argument: this_is_arg1
Second argument: this_is_arg2
```

In addition to using the ```for``` loop and the ```set``` command, we can also access command line arguments using the ``` count ``` and ```argv``` variables. The following code will print the total number of arguments and the arguments themselves:

```
Fish Shell: Accessing Count and argv Variables

#!/usr/bin/env fish

echo "Number of arguments: $count"
echo "All arguments: $argv"

```

Executing ```fish read_args.fish arg1 arg2 arg3``` will output:

```
Number of arguments: 3
All arguments: arg1 arg2 arg3
```

## Deep Dive

Reading command line arguments can be very useful in creating scripts or programs that need user input. It allows for flexibility and customization as the user can provide different input each time the script is run. The ```argv``` array in Fish Shell automatically splits input based on spaces, making it easy to iterate through each argument using a ```for``` loop. Using the ```set``` command to assign arguments to variables can also be helpful in organizing and manipulating the input.

It is important to note that command line arguments are read as strings, so if your program requires a different data type, you will need to convert the input accordingly. Additionally, the first argument in the ```argv``` array is always the name of the script or program being run, so keep that in mind when accessing arguments.

## See Also

- [Fish Shell documentation on command line arguments](https://fishshell.com/docs/current/cmds/set.html#set-variable)
- [Fish Shell tutorial on command line arguments](https://dev.to/danicat/fish-shell-tutorial-reading-command-line-arguments-l5p)
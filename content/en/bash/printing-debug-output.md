---
title:                "Bash recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
As developers, we often encounter errors and bugs in our code that can be difficult to track down. One way to solve these issues is by using debug output. This type of output allows us to see the values of variables and the flow of our program, making it easier to identify and fix any problems.

## How To
To print debug output in Bash, we can use the `echo` and `printf` commands. Let's take a look at an example using `echo`:

```
#!/bin/bash

x=5
y=10

echo "The value of x is $x"
echo "The value of y is $y"
```

In this code, we have two variables `x` and `y`. By using `echo` and passing in the variable names with a `$` symbol, we can print out their values. The output will be:

```
The value of x is 5
The value of y is 10
```

We can also use `printf` for more precision in our output. Let's see an example using `printf`:

```
#!/bin/bash

x=5.123
y=10.456

printf "The value of x is %.2f\n" $x
printf "The value of y is %.2f\n" $y
```

In this code, we are printing the values of `x` and `y` with a precision of 2 decimal places using the `%f` format specifier. The output will be:

```
The value of x is 5.12
The value of y is 10.46
```

## Deep Dive
One important aspect of printing debug output is knowing when and where to use it. It's best to strategically place your `echo` or `printf` statements at different points in your code to get a better understanding of what's happening. You can also use conditional statements to only print the output when certain conditions are met.

Another useful tip is to use colors in your output. This can help differentiate between different types of output and make it easier to read. For example, you can use the `tput` command to change the text color:

```
#!/bin/bash

# This will change the text color to red
tput setaf 1

echo "This is red text."

# This will change the text color back to the default
tput sgr0

echo "This is default text."
```

Output:

```
This is red text.
This is default text.
```

There are many more techniques and tools that can be used for printing debug output in Bash. It's important to experiment and find what works best for you and your code.

## See Also
- [Bash Debugging Techniques](https://medium.com/@pimterry/10-simple-debugging-tips-for-bash-bcb4e66facc1)
- [Debugging in Bash with set -x](https://www.shell-tips.com/bash/debug-script-bash/)
- [Using Debugging Tools in Bash](https://www.linuxjournal.com/content/using-debugging-tools-bash)
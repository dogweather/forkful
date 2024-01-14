---
title:                "Fish Shell recipe: Printing debug output"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming process, and it helps us identify and fix errors in our code. However, sometimes the process can be challenging, especially when dealing with large programs. One way to make debugging easier is by using print statements to output information about the program's execution. This allows us to see the values of variables and the flow of the program, making it easier to identify and fix any issues.

## How To

To print debug output in Fish Shell, we can use the `echo` command. Let's say we have a simple program that calculates the area of a rectangle:

```
Fish Shell 
# Define variables
set width 5
set length 10

# Calculate area
set area ($length * $width)

# Print debug output
echo "Length: $length"
echo "Width: $width"
echo "Area: $area"
```

When we run this program, we will see the following output:

```
Length: 10
Width: 5
Area: 50
```

We can also use the `printf` command to format our output. For example, if we want to add some text to our debug output, we can use the following code:

```
Fish Shell
# Define variables
set width 5
set length 10

# Calculate area
set area ($length * $width)

# Print debug output
printf "The length of the rectangle is: %s \n" $length
printf "The width of the rectangle is: %s \n" $width
printf "The area of the rectangle is: %s \n" $area
```

This will give us the following output:

```
The length of the rectangle is: 10 
The width of the rectangle is: 5 
The area of the rectangle is: 50 
```

Using print statements allows us to see the values of variables at different points in our program's execution and helps us understand the flow of our code.

## Deep Dive

In Fish Shell, we can also use the `sprintf` command to format our output and save it to a variable. For example, if we want to save the debug output of our rectangle program to a variable, we can use the following code:

```
Fish Shell
# Define variables
set width 5
set length 10

# Calculate area
set area ($length * $width)

# Format debug output
set debug_output (sprintf "The length of the rectangle is: %s \n The width of the rectangle is: %s \n The area of the rectangle is: %s \n" $length $width $area)

# Print debug output
echo $debug_output
```

This will give us the same output as our previous example, but now it is saved in the `debug_output` variable, which we can use in our program.

Another useful command for debugging in Fish Shell is the `pstack` command. This allows us to see the current stack trace, which shows us the functions or commands that were called leading up to an error or issue in our code. We can use this to pinpoint the exact location of the problem and debug more efficiently.

## See Also

- [Fish Shell documentation on debugging](https://fishshell.com/docs/current/commands.html#debugging)
- [Fish Shell tutorial on debugging](https://fishshell.com/docs/current/tutorial.html#debugging)
- [Using Debugging Tools in Fish Shell](https://medium.com/swlh/using-debugging-tools-in-fish-shell-fcd508f44e4d)
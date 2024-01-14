---
title:    "Bash recipe: Printing debug output"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Have you ever encountered a bug in your Bash program and found yourself scratching your head trying to figure out what went wrong? Or are you just curious about how your program executes? In either case, adding debug output to your Bash script can be a useful tool in gaining insight into your code's behavior.

## How To

Adding debug output is a simple yet effective way to understand what is happening in your Bash script. To do this, you can use the `echo` command to print out variables, values, or messages throughout your code. Let's take a look at an example:

```Bash
#!/bin/bash

# A Bash program with debug output
echo "Starting program..."

# Setting a variable
message="Hello, World!"

# Printing variable's value
echo "The message is: $message"

# Looping through numbers and printing them
for i in {1..5}
do
  echo "The current number is: $i"
done

# End of program
echo "Program finished."
```

In this code, we use the `echo` command to print out the start and end messages as well as the value of the `message` variable and the current iteration of the loop. This allows us to see exactly what is happening in our code and identify any potential issues.

When running this program, our output will look like this:

```Bash
Starting program...
The message is: Hello, World!
The current number is: 1
The current number is: 2
The current number is: 3
The current number is: 4
The current number is: 5
Program finished.
```

Adding debug output can also be helpful when working with conditional statements or functions. For example, if we want to see which branch of an `if` statement is being executed, we can add a debug statement after each condition.

```Bash
if [ "$num" -gt 10 ]; then
  echo "Condition 1 met"
elif [ "$num" -lt 0 ]; then
  echo "Condition 2 met"
else
  echo "Neither condition met"
fi
```

## Deep Dive

Adding debug output not only helps with troubleshooting, but it can also improve the readability of your code. However, using `echo` statements for debugging can get messy and clutter up your code. A better option is to use the `set -x` command at the beginning of your script. This will print out a trace of every command that is executed, making it easier to follow the flow of your code.

Another useful technique is to add a debugging flag to your script. This allows you to easily turn on or off the debug output with a single change instead of having to manually remove or comment out each `echo` statement. Here's an example of how you can implement this:

```Bash
#!/bin/bash

# Check if the debug flag is set
if [ "$DEBUG" = "true" ]; then

  # Enable debugging
  set -x

fi

# Rest of your code goes here
message="Debug output enabled"
echo "$message"

# Disable debugging
set +x
```

By setting `DEBUG` to `true` before running the script, we enable debug output and can see every command that is executed. Once we're done debugging, we can simply set `DEBUG` to `false` and the `set -x` command will be ignored.

## See Also

- [Bash Debugging Techniques](https://linuxhint.com/bash_debugging_techniques/)
- [Debugging Bash Script with set -x](https://www.tecmint.com/debug-linux-bash-scripts-set-e-set-x-options/)
- [Bash Debugger: A Bash Debugger-Like Function with Colors (and No External Debugging Tools)](https://www.shell-tips.com/bash/debug-script-bash-debugger-colors/)

Debug output can be a valuable tool in understanding your Bash script's behavior and identifying bugs. Whether you choose to manually add `echo` statements or use the `set -x` command, it's a simple yet effective way to improve your coding and problem-solving skills. Happy debugging!
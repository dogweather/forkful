---
title:                "Printing debug output"
html_title:           "Bash recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of the coding process, helping developers identify and fix errors in their code. Printing debug output is a useful way to get a closer look at what is happening during program execution, making it easier to troubleshoot and resolve any issues.

## How To
To print debug output in Bash, you can use the `echo` command. This command displays a message on the terminal, which can be useful for debugging purposes. Let's look at an example:

```Bash
#!/bin/bash
# Simple Bash script to demonstrate debug output

# Set debug mode to 1
DEBUG_MODE=1 

# Print debug output if DEBUG_MODE is set to 1
if [ $DEBUG_MODE -eq "1" ]; then
  echo "Debug mode is ON"
fi

# Run the rest of the script
echo "Welcome to my program!"
```

In the above code, we have set a variable `DEBUG_MODE` to 1, indicating that debug mode is on. If the condition is true, the `echo` command will print the message `Debug mode is ON` on the terminal. This can be particularly useful when you have large and complex scripts, as it allows you to track the execution and pinpoint any issues.

## Deep Dive
In Bash, you can also use the `set -x` command to enable tracing mode, which will print out each command before it is executed. This can be even more useful for debugging as it gives you a closer look at the sequence of commands being executed. To turn off tracing mode, you can use the `set +x` command.

Another helpful tool for printing debug output is the `printf` command, which allows you to format the output in a specific way. This can be helpful when you need to display specific variable values or need to format the output in a particular way for debugging purposes.

Overall, printing debug output in Bash can significantly aid in troubleshooting and identifying any issues in your code. It allows you to track the execution and get a closer look at what is happening, making the debugging process more efficient.

## See Also
- [Bash Debugging Techniques](https://linuxize.com/post/bash-debugging-techniques/)
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Debugging Shell Scripts](https://www.tecmint.com/debugging-shell-scripts/)
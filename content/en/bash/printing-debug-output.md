---
title:    "Bash recipe: Printing debug output"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential skill for any programmer, and printing debug output is a valuable technique that can save you time and frustration in the long run. By printing out specific values or messages during the execution of your code, you can track and understand how your code is running and identify any potential errors. So, if you want to become a more efficient and effective programmer, knowing how to print debug output is a must.

## How To

Printing debug output in Bash is a straightforward process. You can use the built-in `echo` command to print out any desired text or variable value. Here's a simple example:

```Bash
# Declare a variable
name="John"

# Print out a message
echo "Hello, $name! It's nice to meet you."
```

The output of this code would be:

```Bash
Hello, John! It's nice to meet you.
```

Another useful command for debugging is `printf`, which allows you to format the output in a specific way. Here's an example:

```Bash
# Declare a variable
age=25

# Print out a formatted message
printf "I am %d years old.\n" "$age"
```

The output of this code would be:

```Bash
I am 25 years old.
```

You can also use these commands within functions to print out debug messages for different parts of your code. For example:

```Bash
# Define a function
say_hello() {
  name="Lisa"
  echo "Hello, $name!"
}

# Call the function
say_hello
```

The output of this code would be:

```Bash
Hello, Lisa!
```

## Deep Dive

Now let's dive a bit deeper into printing debug output in Bash. One thing to keep in mind is that when you're printing out variables, you should always use double quotes around the variable name to ensure that the value is actually printed instead of the name. For example:

```Bash
# Declare a variable
my_var="Hello"

# This will print "my_var"
echo "$my_var"

# This will print "Hello"
echo $my_var
```

Furthermore, you can also use the `set -x` command at the beginning of your Bash script to turn on debugging mode. This will print out the executed command and its arguments, which can be helpful in tracking down errors.

Another useful tool for debugging is the `test` command, which allows you to test conditions and print out the results. For example:

```Bash
# Test if a variable is set
test -n "$my_var" && echo "my_var is set" || echo "my_var is not set"
```

The output of this code would be:

```Bash
my_var is set
```

## See Also

- [Bash Debugging Techniques](https://www.linuxjournal.com/content/debugging-bash-scripts)
- [Debugging Bash Scripts with xtrace](https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html#The-Set-Builtin)
- [Using the Test Command](https://www.tldp.org/LDP/abs/html/testconstructs.html)
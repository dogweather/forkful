---
title:    "Fish Shell recipe: Reading command line arguments"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to run a program with specific options or input every time you use it? Or have you ever wanted to write a script that takes in user input from the command line? If so, learning how to read command line arguments in Fish Shell can greatly simplify your workflow and make your scripts more flexible.

## How To

To read command line arguments in Fish Shell, we first need to understand how they are passed into a program. When a program is executed from the command line, any words following the program name are considered to be arguments. For example, if we have a program called `greet` and we execute it with the following command: `greet Hello world!`, "Hello" and "world!" would be considered as separate arguments.

Now, let's see how we can access these arguments within our Fish Shell program. The arguments are stored in the `$argv` variable as an array. We can use the `count` command to see how many arguments were passed in, and the `echo` command to print them out. 

```Fish Shell 
# Print out the number of arguments
count $argv 
# Print out each argument on a separate line
for arg in $argv
    echo $arg 
end
```

If we run our `greet` program from earlier with the command `greet Hello world!`, our output would be:

```
2
Hello
world!
```

Notice how the first element in the `$argv` array is always the name of the program itself. This means that in order to access only the user-provided arguments, we need to start at index 1 instead of 0. For example, if we wanted to print out only the arguments and not the program name, we could use this code:

```Fish Shell
for arg in $argv[1..-1]
    echo $arg
end
```

Now our output would just be:

```
Hello
world!
```

## Deep Dive

There are a few other ways to manipulate and access command line arguments in Fish Shell. 

### Positional Arguments

We've already seen how to access arguments using the `$argv` array, but did you know that we can also refer to specific arguments by their position within that array? For example, if we wanted to access only the first argument, we could use the syntax `$argv[1]`. This can come in handy when you know exactly which argument you need to work with.

### Flags and Options

Sometimes, we may want to provide additional information to our program through the use of flags or options. These are typically preceded by a `-` or `--` and can be accessed using the `$flag` or `$options` arrays respectively. For example, if we ran our `greet` program with the command `greet -f John`, we could access the flag "f" and its value "John" with the code:

```Fish Shell
# Print out the value of the "f" flag
echo $flags["f"] 
```

Similarly, if we wanted to provide options to our program, we would use the format `program_name -option1 value1 -option2 value2 ...`, and then access those options in our program using the `$options` array.

## See Also

- [Fish Shell documentation on arguments](https://fishshell.com/docs/current/tutorial.html#arguments)
- [An introduction to Fish Shell scripting](https://sunfishcode.github.io/posts/learn-fish-shell-pt4)
- [How to pass command line arguments in Fish Shell scripts](https://docs.fishsoup.net/how-to-pass-command-line-arguments)
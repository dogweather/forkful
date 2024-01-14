---
title:                "Java recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

In Java programming, command line arguments are often used to pass input parameters to a program before it executes. Understanding how to read and handle command line arguments can greatly enhance your programming skills and improve the functionality of your programs.

## How To

To read command line arguments in Java, we will use the `args` parameter in the `main()` method. This parameter is an array of strings that contains the arguments passed to the program. Let's take a look at a simple example where we pass two arguments, "John" and "Doe", to our program:

```Java
public class CommandLineArgs {

    public static void main(String[] args) {
        System.out.println("Hello " + args[0] + " " + args[1]);
    }
}
```

The output for this program would be:

```
Hello John Doe
```

You can pass multiple arguments to the program, just make sure to separate them with a space. You can also access each argument separately by using its index in the `args` array.

## Deep Dive

There are a few things to keep in mind when working with command line arguments in Java. First, if no arguments are passed to the program, the `args` array will be empty. Therefore, it's important to validate the length of the `args` array before trying to access its elements.

Second, command line arguments are always passed as strings, even if they are numbers. This means that you will need to convert them to the appropriate data type before using them in your program. For example, if you want to use an argument as an integer, you would need to use the `Integer.parseInt()` method to convert it from a string to an integer.

Finally, if an argument contains spaces, it will need to be enclosed in double quotation marks when passing it to the program. For example, if I wanted to pass the argument "Hello World" to my program, I would need to pass it as "Hello World" in the command line.

## See Also

To learn more about command line arguments in Java, check out the following resources:

- [Java Command Line Arguments tutorial](https://www.baeldung.com/java-command-line-arguments)
- [Oracle documentation on Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Passing Command Line Arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)

Keep practicing and experimenting with command line arguments in Java to become more proficient in using them in your programs. Happy coding!
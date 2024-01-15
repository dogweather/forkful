---
title:                "Reading command line arguments"
html_title:           "Java recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Command line arguments are important for allowing users to pass information to a Java program during runtime. This enables more flexibility and customization, making it a useful feature for both developers and end users.

## How To

To read command line arguments in Java, follow these steps:

1. Start by creating a main method, which is required in all Java programs.
2. Within the main method, declare a String array parameter called `args`. This is where the command line arguments will be stored.
3. Use the `args` array to access the command line arguments. The first argument will be stored in `args[0]`, the second in `args[1]`, and so on.
4. You can then use these arguments in your program's logic to provide different functionality based on user input.
5. To run your program with command line arguments, navigate to the directory where your Java file is located and use the `java [filename] [arguments]` command. All arguments should be separated by a space.

Here is an example code block showing how to read and use command line arguments in Java:

```Java
public static void main(String[] args) {

    // check if arguments were provided
    if (args.length > 0) {
        // print out the first argument
        System.out.println("The first argument is: " + args[0]);
    } else {
        System.out.println("No arguments provided.");
    }
}
```

Sample output when running the code with `java CommandLineArguments hello`:

`The first argument is: hello`

## Deep Dive

There are a few important things to note when working with command line arguments in Java:

- The `args` array will always be at least 1 element long, even if no arguments are provided. This is because the name of the Java program itself is always considered the first argument.
- Arguments passed through the command line are always treated as strings, so be sure to use appropriate type casting if you need to use the arguments as other data types.
- It is possible to provide multiple arguments when running a Java program. These will be stored in the `args` array in the order they were passed in.
- Command line arguments can also be used with jar files. Simply use the `java -jar [filename].jar [arguments]` command to run the jar file with the specified arguments.

## See Also

- [Oracle's official documentation on command line arguments in Java](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Stack Overflow discussion on using command line arguments in Java](https://stackoverflow.com/questions/890966/what-is-string-args-parameter-in-main-method-java)
- [GeeksforGeeks article on using command line arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
---
title:                "Java recipe: Reading command line arguments"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Why

Have you ever wondered how to handle command line arguments in your Java programs? Well, look no further! By learning this skill, you'll be able to make your programs more dynamic and user-friendly. Keep reading to find out how.

# How To

```Java 
public class CommandLineArgs {
    public static void main(String[] args) {
        System.out.println("Welcome to my program!");
        //reading in command line arguments
        for (int i = 0; i < args.length; i++) {
            System.out.println("Argument " + (i+1) + ": " + args[i]);
        }
    }
}
```

If you run this program with some arguments, you'll see them printed out on the console. For example, if you run it with `java CommandLineArgs arg1 arg2`, you'll see the following output:

```
Welcome to my program!
Argument 1: arg1
Argument 2: arg2
```

The `String[] args` in the `main` method is an array that holds the arguments passed in from the command line. You can access each argument by its index, starting at 0 for the first argument.

# Deep Dive

There are a few things to keep in mind when dealing with command line arguments in Java. Here are some tips:

- The first argument (`args[0]`) is always the name of the program.
- Arguments are read in as strings, so if you need them as other data types, you'll need to convert them.
- You can use the `java -cp . <class name>` command to specify the classpath when running Java programs with command line arguments. 

# See Also

- [Oracle Documentation on Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [GeeksforGeeks article on Command Line Arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
- [Java Command Line arguments - How To Geek tutorial](https://www.howtogeek.com/284686/how-to-use-command-line-arguments-in-java/)
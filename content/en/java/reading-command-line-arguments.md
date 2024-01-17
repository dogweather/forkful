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

## What & Why?
Reading command line arguments is a process of extracting user-inputted data from the command line interface in order to use it in a Java program. This is commonly done by programmers to make their programs more interactive and customizable, allowing users to input specific values or options.

## How to:
To read command line arguments in Java, we use the "args" parameter in the main method. Here's a simple example of a program that reads two arguments and prints them out:

```
public static void main(String[] args) {
  System.out.println("Arguments entered: " + args[0] + " and " + args[1]);
}
```

Running this program with command line inputs "Hello" and "world" would print: "Arguments entered: Hello and world". Another way to access command line arguments is by using the Scanner class, as shown in the following code:

```
import java.util.Scanner;

public class CommandLine {
  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    System.out.println("Enter your name: ");
    String name = sc.nextLine();
    System.out.println("Hello " + name + "!");
  }
}
```

This program prompts the user to enter their name and then prints a greeting using that input.

## Deep Dive:
Reading command line arguments has been a part of programming since the inception of Java in 1995. This feature allows programs to become more dynamic and user-friendly. Alternative methods include using system properties, environment variables, or configuration files. 

To read command line arguments, Java uses the "String[]" data type, representing an array of String objects. We can also use the "java.util.Scanner" class to access command line inputs. This class provides methods for reading different data types, making it more versatile for handling user input.

## See Also:
- [Java String[]](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Scanner class](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Command Line Arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
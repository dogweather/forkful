---
title:                "Printing debug output"
html_title:           "Java recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of the coding process, and being able to print debug output can be incredibly helpful in identifying and fixing issues. It allows you to see the specific values of variables and functions at different points in your code, providing valuable insights into the functioning of your program.

## How To
To print debug output in Java, you can use the `System.out.println()` method. This method takes in a string or variable and prints it to the console. Let's say we want to print the value of a variable `num`:

```Java
int num = 10;
System.out.println(num); // will print 10 to the console
```

You can also concatenate strings and variables using the `+` operator:

```Java
String name = "John";
System.out.println("My name is " + name); // will print "My name is John" to the console
```

If you want to print multiple variables or strings on the same line, you can use the `System.out.print()` method instead, which will not add a newline character at the end:

```Java
int num1 = 5;
int num2 = 7;
System.out.print("The sum of " + num1 + " and " + num2 + " is: ");
System.out.println(num1 + num2); // will print "The sum of 5 and 7 is: 12" on one line
```

You can also print the value of expressions to see their result:

```Java
System.out.println("2 + 3 = " + (2 + 3)); // will print "2 + 3 = 5" to the console
```

## Deep Dive
There are a few other ways to print debug output in Java, such as using the `Logger` class or external libraries, but for most cases, `System.out.println()` and `System.out.print()` are sufficient. However, it's important to note that printing debug output can slow down your program, so it's essential to remove it when you're done debugging.

Another useful tip is to use the `java.util.logging.Level` class to specify the level of the debug output. This allows you to control which debug statements are shown based on their level of importance.

## See Also
- [Java Documentation on System.out.println](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html#println())
- [Tutorial on Debugging in Java](https://www.baeldung.com/java-debugging)
- [Using Java's Logger Class for Debugging](https://www.logicbig.com/tutorials/core-java-tutorial/logging/using-java-logging-logger-for-application-logging.html)
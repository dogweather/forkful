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

## What & Why?
Printing debug output is a common practice among programmers to help identify and fix errors in their code. It involves displaying the values of variables and messages during runtime to track the flow of the program and identify potential issues.
By printing debug output, programmers can troubleshoot their code and understand how their program is behaving, helping them to find and fix any bugs in their code more efficiently.

## How to:
To print debug output in Java, we can use the built-in `System.out.println()` method. This method takes in a string or variable and prints it to the console. Let's look at an example below:

```Java
// Debug output example
public class DebugOutput {
  public static void main(String[] args) {
    String name = "John";
    int age = 25;

    // Print debug output
    System.out.println("Name: " + name);
    System.out.println("Age: " + age);
  }
}
```

This will output the following:

```
Name: John
Age: 25
```

We can also use `System.out.printf()` to format our output. This method allows us to specify the format of our output, which can be helpful when debugging complex data types. Here's an example:

```Java
// Debug output with formatting
public class DebugOutputFormat {
  public static void main(String[] args) {
    int num = 123456789;
    double price = 19.99;
    boolean isReady = true;

    // Print debug output with formatting
    System.out.printf("Number: %d%n", num);
    System.out.printf("Price: $%.2f%n", price);
    System.out.printf("Is ready? %b%n", isReady);
  }
}
```

The output will be:

```
Number: 123456789
Price: $19.99
Is ready? true
```

## Deep Dive:
Historically, printing debug output was commonly used in older programming languages like C and Assembly, as it was the only way to track the flow of the program. However, with the introduction of modern integrated development environments (IDEs), debugging tools like breakpoints, watches, and step-by-step execution have become more popular.
But, printing debug output still remains a useful technique, especially for quick and informative debugging during development. Alternative methods like using a debugger may require more setup and may not be as flexible as printing debug output.

When printing debug output, it's important to only include relevant information to avoid cluttering the output. You can use conditional statements and control flow to selectively print debug output for specific parts of your code. This can help save time and make it easier to pinpoint and fix issues.

## See Also:
- [Debugging in Java - Tutorialspoint](https://www.tutorialspoint.com/java/java_debugging.htm)
- [System Class in Java - GeeksforGeeks](https://www.geeksforgeeks.org/system-class-in-java/)
- [Debugging with System.out.println() - Baeldung](https://www.baeldung.com/java-system-out-println-debugging)
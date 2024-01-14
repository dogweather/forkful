---
title:    "Java recipe: Writing to standard error"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
When writing a Java program, it's important to know how to handle errors and debugging. One way to do this is by utilizing the standard error stream. Writing to standard error allows for any errors or exceptions in the code to be displayed, making it easier to identify and solve issues during development.

## How To
To write to standard error in Java, you will need to use the "System.err" object and the "println()" method. This method is similar to the regular "System.out.println()" method, but it prints to standard error instead of the standard output. Let's look at an example:

```Java
public class ErrorExample {
    public static void main(String[] args) {
        System.err.println("This is an error message");
        System.out.println("This is a regular output");
    }
}
```
When running this code, the error message will be displayed in red text while the regular output will be displayed in default color. This makes it easy to differentiate between the two.

```
This is an error message
This is a regular output
```

You can also use the "System.err" object to print the errors and exceptions thrown by the program. For example:

```Java
public class DivideByZero {
    public static void main(String[] args) {
        int num = 10;
        int divideBy = 0;
        try {
            int result = num / divideBy;
        } catch (ArithmeticException e) {
            System.err.println("Cannot divide by zero");
        }
    }
}
```
In this case, since we are dividing by zero, the program will throw an ArithmeticException. However, by using the "System.err" object, we are able to catch the exception and display a meaningful error message to the user.

```
Cannot divide by zero
```

## Deep Dive
When writing to standard error, it's important to keep in mind the difference between the standard output and standard error streams. The standard output stream is used for regular program output while the standard error stream is used for error messages and debugging information.

Another thing to note is that the standard error stream is unbuffered, meaning it is printed immediately when called. This is different from the standard output stream, which is buffered and may not be displayed until the buffer is flushed or the program terminates.

Additionally, it's worth mentioning that the "System.out" and "System.err" objects are not limited to just printing to the console. They can also be used to write to a file or another output stream, making them even more versatile in handling program errors.

## See Also
- [Java Exceptions Tutorial](https://www.baeldung.com/java-exceptions)
- [The Java Tutorials: Handling Errors](https://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)
- [Java Standard Error Stream Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)

By utilizing the standard error stream in your Java programs, you can easily display error messages and exceptions, making it easier to debug and improve your code's functionality. Don't underestimate the power and importance of writing to standard error in your development process.
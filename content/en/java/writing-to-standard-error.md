---
title:    "Java recipe: Writing to standard error"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why

When writing code in Java, you may come across a situation where you need to display an error message to the user. This can be done using the "standard error" stream, also known as "stderr". Writing to standard error allows you to provide important information about any potential errors or issues that may occur during program execution. It is a crucial tool for debugging and troubleshooting in Java programming.

## How To

To write to standard error in Java, you can use the `System.err` object. This object is of type `PrintStream` and provides methods to display messages to the standard error stream.

Let's look at an example:

```Java
public static void main(String[] args){
    System.err.println("This is an error message");
}
```

In the above code, we are using the `println()` method of the `System.err` object to print a message to the standard error stream. This will display the message in the console with a red font, indicating that it is an error. 

You can also use the `printf()` method to format your error messages. For example:

```Java
public static void main(String[] args){
    System.err.printf("Error: The value of x is %d, which is not valid.", 5);
}
```

This code will display the error message along with the value of the variable `x`.

It is important to note that when writing to standard error, the output will always be displayed in the console, even if you have redirected the standard output to a file. This makes it a useful tool for displaying errors in command-line applications.

## Deep Dive

When using the `System.err` object, you may come across the `err` field and the `setError()` method. These are used to specify where the standard error stream should be redirected. By default, the standard error stream is directed to the console, but you can change this behavior by using these fields and methods.

For example, if you want to redirect the standard error stream to a file, you can do so by using the `setError()` method:

```Java
public static void main(String[] args){
    try {
        System.err.setError(new PrintStream(new File("error.log")));
        System.err.println("This is an error message");
    } catch (FileNotFoundException e) {
        e.printStackTrace();
    }
}
```

In the above code, we are redirecting the standard error stream to a file named "error.log". This will store all the error messages in the specified file instead of displaying it in the console.

You can also use the `err` field to redirect the standard error stream to a different output stream, such as a socket or an output stream.

## See Also

- [Java PrintStream documentation](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html)
- [Java File class documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java Exceptions and Error Handling guide](https://www.baeldung.com/java-exceptions)

Writing to standard error is an important aspect of Java programming and can greatly help in debugging and troubleshooting your code. It is a simple yet powerful tool that every Java developer should be familiar with.
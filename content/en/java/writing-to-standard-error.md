---
title:                "Java recipe: Writing to standard error"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

When developing Java programs, it is important to have a way to handle error and exception messages. Writing to standard error allows developers to display more detailed and specific error messages to help with troubleshooting and debugging. It is also a common practice to write to standard error when dealing with server or console applications.

## How To

Writing to standard error in Java is a simple process. First, import the necessary packages:

```Java
import java.io.FileOutputStream;
import java.io.PrintStream;
```

Then, create a PrintStream object that points to the standard error output stream:

```Java
PrintStream err = System.err;
```

After that, you can use the `println` method to write your error message:

```Java
err.println("Error: File not found.");
```

This will print the message to the standard error output. You can also use the `printf` method to format the message:

```Java
err.printf("Error: Invalid input '%s' on line %d.", userInput, lineNum);
```

The above code will print a formatted error message with the user's input and the line number where the error occurred.

## Deep Dive

Standard error is a separate output stream from standard output (`System.out`) and is used specifically for error and exception messages. It is useful to separate error messages from regular program output so that they can be easily identified and handled accordingly.

In addition, developers can redirect the standard error stream to a file instead of the console by using the `setErr` method:

```Java
System.setErr(new PrintStream(new FileOutputStream("error.log")));
```

This will direct all error messages to the specified file, allowing for easier tracking and analysis of errors in a production environment.

## See Also
- [Java PrintStream Documentation](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html)
- [Java System class Documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html)
- [Understanding Error Messages in Java](https://www.baeldung.com/java-error-messages)
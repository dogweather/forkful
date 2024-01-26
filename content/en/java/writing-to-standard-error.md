---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) is a way to output error messages and diagnostics separate from standard output (stdout). Programmers use it to signal that something exceptional has occurred, making it easier to debug and isolate issues.

## How to:

Java makes writing to stderr simple using `System.err`. Here's a quick look:

```java
public class StderrExample {
    public static void main(String[] args) {
        System.err.println("Error: Something went wrong!");
    }
}
```

Running this gives you:

```
Error: Something went wrong!
```

Note: While stdout usually goes to the console, stderr can be redirected to a file or other destination, keeping error messages separate.

## Deep Dive

Historically in Unix-like systems, stderr is file descriptor 2, distinct from stdout (file descriptor 1). This allows for different handling and redirection. Alternatives to `System.err` include logging frameworks like Log4J or SLF4J, which offer more features. In Java, stderr is implemented in the `System` class and is an instance of `PrintStream`. It's unbuffered, meaning output is immediate.

## See Also

- [Oracle Java Docs - System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Wikipedia - Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
- [Tutorial on Java Logging](https://www.baeldung.com/java-logging-intro)

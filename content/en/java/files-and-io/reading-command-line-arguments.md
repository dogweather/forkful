---
date: 2024-01-20 17:55:59.874250-07:00
description: "Reading command line arguments in Java is snagging input provided by\
  \ users when they launch your program from a console. Programmers do it to make\
  \ their\u2026"
lastmod: '2024-03-11T00:14:33.849642-06:00'
model: gpt-4-1106-preview
summary: "Reading command line arguments in Java is snagging input provided by users\
  \ when they launch your program from a console. Programmers do it to make their\u2026"
title: Reading command line arguments
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in Java is snagging input provided by users when they launch your program from a console. Programmers do it to make their apps responsive to user needs, flexibly handling tasks without hardcoded values.

## How to:

Java scoops up command line arguments you dish out with the `main` method. Look at this bite-sized sample:

```java
public class CommandLineExample {
    public static void main(String[] args) {
        // Let's print out the command line arguments
        for(String arg : args) {
            System.out.println(arg);
        }
    }
}
```

Fire up your terminal, compile with `javac CommandLineExample.java`, and run with `java CommandLineExample These Are Command Line Arguments`. Here's your output:

```
These
Are
Command
Line
Arguments
```

## Deep Dive

Originating from C, command line arguments have been a staple since the dark ages of programming—think punch cards and timesharing. Java inherited this utility for good reason. It's elementary, versatile, and fits a range of situations.

Highly alternative? Sure, there's plenty. Libraries like JCommander or Apache Commons CLI beef up your parsing prowess. They handle more complex scenarios with finesse.

Under the hood, Java's `main` method snags a `String` array—`args`. In the virtual machine run, when you hit `java ClassName`, what follows are your inputs, neatly stored in `args`.

## See Also:

- For a refresher on the basics: [Oracle's official Java tutorials](https://docs.oracle.com/javase/tutorial/)
- Dive into JCommander for complex parsing: [JCommander GitHub](https://github.com/cbeust/jcommander)
- Explore Apache Commons CLI: [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)

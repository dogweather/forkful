---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in Java is the act of accessing input values sent to your program during its initiation. Why bother? Well, it offers flexibility and control by letting you run a program with different parameters without needing to modify its code.

## How to:

Too eager to begin! Here's your map.

```Java
public class Main {
    public static void main(String[] args) {
        for(String arg:args) {
            System.out.println("Argument value: " + arg);
        }
    }
}
```

Run the program using `java Main apple banana cherry`. The output? 

```
Argument value: apple
Argument value: banana
Argument value: cherry
```
Neat, huh?

## Deep Dive

On a historical note, command-line argument handling has been a standard even before Bill Gates was out of diapers. It's a feature in just about every programming language worth its weight in silicon. 

Java support for command line arguments? It arrived right at its birth in Incubator – packaged in an array of `Strings`, which can be accessed like any other array in Java.

Alternatives – you bet. Java libraries like Apache Commons CLI and JCommander help handle more complex command-line argument scenarios. 

Diving deeper into implementation, the `main` method in Java accepts an array of `Strings` as a parameter, which hoards these arguments. When you run your program from your command console, arguments are separated by spaces and counted as different elements of the `String`.

## See Also

For more insights, put your snorkel on and dive in the ocean of these resources:

- Understanding command line arguments in depth: [IBM’s guide to Java’s main method](https://www.ibm.com/developerworks/java/library/j-dcl/index.html)
- Need a variation of the theme? Explore JCommander: [JCommander guide](http://jcommander.org/)
 
Life made easy! Happy coding!
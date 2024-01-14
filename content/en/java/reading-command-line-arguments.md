---
title:    "Java recipe: Reading command line arguments"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why

As a Java programmer, you may have encountered situations where you need to pass arguments to your program. Understanding how to read command line arguments can greatly improve your productivity and make your programs more user-friendly.

## How To

Reading command line arguments is a fairly simple process in Java. Here is a basic example of how to do it:

```Java
public class CommandLineArgs {

    public static void main(String[] args) {
        
        // Check if any arguments were passed
        if (args.length > 0) {
            System.out.println("The following arguments were passed:");
            
            // Loop through and print out each argument
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("No arguments were passed.");
        }
    }
}
```

**Sample Output**

If we run this program with the arguments "Hello" and "World", the output will be:

```
The following arguments were passed:
Hello
World
```

You can also pass multiple arguments at once, separated by spaces.

## Deep Dive

To better understand the process of reading command line arguments, let's take a closer look at the code example above.

First, we create a public class named "CommandLineArgs". Then, within the "main" method, we check if any arguments were passed by using the "args.length" property. This property represents the number of arguments passed.

If there are arguments passed, we use a for-each loop to iterate through each argument and print it out. If no arguments were passed, we simply print out a message stating so.

It's important to note that command line arguments are read as String arrays, so you can perform any necessary parsing or conversions as needed.

## See Also

- [Oracle's documentation on Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [GeeksforGeeks tutorial on Command Line Arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
- [Tutorialspoint's explanation of Main method and Command Line Arguments in Java](https://www.tutorialspoint.com/main-method-and-command-line-arguments-in-java)

Reading command line arguments may seem like a small topic, but it can greatly enhance your abilities as a Java programmer. So the next time you encounter a situation where you need to pass arguments to your program, remember this post and use it as a reference. Happy coding!
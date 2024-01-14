---
title:    "Java recipe: Reading command line arguments"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

### Why

Have you ever wondered how programs like the Command Prompt or Terminal are able to take in specific instructions from users? That's because of the use of command line arguments, which can be incredibly useful for coding in Java. In this blog post, we'll dive into the world of command line arguments and how they can benefit your programming skills.

### How To
To read command line arguments in Java, we use the ```args``` parameter in the main method. Let's take a look at a simple example:

```Java
public class CommandLineArgs {

   public static void main(String[] args) {
      
      for (String arg : args) {
         System.out.println(arg);
      }
   }
}
```

This code creates a basic Java class with a main method that takes in an array of strings called ```args```. In the main method, we use a for loop to iterate through the ```args``` array and print out each argument. Now, let's compile and run this code with some arguments:

```
$ javac CommandLineArgs.java
$ java CommandLineArgs Hello World!
```

The output would be:

```
Hello
World!
```

As you can see, the arguments we passed in (Hello and World!) were printed out in the same order that we entered them. This is just a simple example, but you can use command line arguments to create more complex programs that take in input from users.

### Deep Dive
Now, let's take a deeper look at how command line arguments work. When we run a Java program with command line arguments, we are basically passing in data to the program. The ```args``` parameter is an array of strings, so each individual argument is stored as a string in the array. This allows us to manipulate and use the arguments in our code.

One thing to note is that command line arguments are always passed in as strings, even if they were originally entered as numbers. This means we may need to convert the arguments to a different data type if we want to perform calculations or comparisons with them. We can do this using the respective wrapper class, such as ```Integer.valueOf()``` for converting to an integer.

Another thing to keep in mind is that the order of the arguments matters. The first argument entered will be at index 0 in the ```args``` array, the second argument will be at index 1, and so on. This can be helpful when we want to use certain arguments for specific purposes in our program.

### See Also

* [Oracle Java Docs: Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
* [GeeksforGeeks: Command Line Arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
* [Tutorialspoint: Java - Command Line Arguments](https://www.tutorialspoint.com/java/java_command_line_arguments.htm)

In conclusion, knowing how to read command line arguments in Java can greatly enhance your programming skills and allow you to create more interactive and useful programs. So next time you're using the Command Prompt or Terminal, remember the power and versatility of command line arguments in Java. Happy coding!
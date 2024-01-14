---
title:                "C# recipe: Printing debug output"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

##Why 

As a programmer, you have probably encountered errors in your code at some point. When this happens, it can be helpful to print out certain values or messages to track down the source of the problem. This is where printing debug output comes in handy. It allows you to see the inner workings of your code and identify any issues that may be causing errors. In this blog post, we will discuss the importance of printing debug output in C# programming.

##How To 

To print debug output in C#, we can use the `Console.WriteLine()` method. This method takes in a string as its input and displays it in the console window. Let's take a look at an example:

```C#
//Printing a simple message
string message = "Hello World!";
Console.WriteLine(message);

//Printing variable values
int number = 5;
Console.WriteLine("The value of number is: " + number);

//Formatting output with placeholders
double salary = 5000.50;
Console.WriteLine("My monthly salary is: ${0}", salary);
```

The above examples demonstrate how we can use the `Console.WriteLine()` method to print out different types of data in the console window. This can be useful for debugging purposes as well as general information output for the user.

##Deep Dive 

Now, let's dive deeper into printing debug output. In addition to the `Console.WriteLine()` method, C# also provides the `Debug.WriteLine()` method for printing debug messages. The main difference between these two methods is that the `Debug.WriteLine()` method is only executed when the code is in debug mode, whereas `Console.WriteLine()` is executed regardless of the build mode.

Another useful feature of the `Debug.WriteLine()` method is that you can pass in an additional parameter to specify the name of the file and line number where the debug message is coming from. This can be helpful for tracking down which part of the code is causing the error.

Additionally, you can use conditional statements along with the `Debug.WriteLine()` method to only print out the debug message when a certain condition is met. This can save us from cluttering the console window with excessive debug messages.

##See Also 

- [C# Debugging Techniques](https://docs.microsoft.com/en-us/visualstudio/debugger/debug-csharp)
- [Debugging in Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-in-visual-studio)
- [Using Output and Debug Windows in Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/using-the-output-and-debug-windows-in-visual-studio?view=vs-2019)

In conclusion, printing debug output is an important aspect of C# programming, especially when it comes to troubleshooting errors. With the `Console.WriteLine()` and `Debug.WriteLine()` methods, we can easily print out relevant information and track down the source of any issues in our code. Give it a try in your next programming project and see how it can improve your debugging process!
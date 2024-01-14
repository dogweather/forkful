---
title:    "C# recipe: Printing debug output"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why 

Have you ever come across a bug in your code and spent hours trying to figure out where it went wrong? Or maybe you just wanted to better understand the flow of your program. Whatever the reason may be, printing debug output is a useful tool for any programmer.

## How To

To print debug output in C#, all you need is the `Debug` class from the `System.Diagnostics` namespace. This class provides useful methods for printing debug output, such as `Debug.WriteLine()` and `Debug.Assert()`. Let's look at an example:

```
C# using System.Diagnostics;

int num = 5;
Debug.WriteLine("The value of num is: " + num);
```

This will print the following output to the console:

```
The value of num is: 5
```

You can also use the `Debug.Assert()` method to check for a certain condition in your code. For example:

```
C# using System.Diagnostics;

int num = 10;
Debug.Assert(num < 5, "num should be less than 5");
```

If the condition is not met, the program will halt and an error message will be displayed. This can be useful for catching any unexpected values or behaviors in your code.

## Deep Dive

Printing debug output is not just limited to printing variables. You can also use it to print out messages or information about the flow of your program. For example, you can add `Debug.WriteLine()` statements at different points in your code to see the order in which they are executed.

Another useful feature is the ability to enable or disable debug output based on build configurations. This allows you to easily turn off debug output in production code without having to remove any code manually.

Additionally, you can use logging frameworks such as log4net or NLog to handle your debug output in a more organized and customizable way. These frameworks also provide options for writing to files or databases, which can be useful for debugging in a production environment.

## See Also

- [Debug Class (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=netcore-3.1)
- [Debugging Techniques in Visual Studio for C#](https://docs.microsoft.com/en-us/visualstudio/debugger/features-and-ways-to-debug-in-visual-studio?view=vs-2019)
- [log4net Tutorial for C# Logging](https://stackify.com/log4net-guide-dotnet-logging/)
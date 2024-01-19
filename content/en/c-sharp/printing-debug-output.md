---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Printing Debug Output in C#: A Quick Overview

## What & Why?

Printing debug output is the process of displaying special messages, variable states, and other data points, which is not typically visible to the end user. It's important for programmers because it helps in understanding and troubleshooting the behaviour of their code during development and testing phases.

## How to:

In C#, the `System.Diagnostics.Debug` class is commonly used for printing debug output. Here's how you can do it:

```C#
using System.Diagnostics;

class Program
{
    static void Main(string[] args)
    {
        int number = 5;
        Debug.WriteLine("Starting program");
        Debug.WriteLine("The number is " + number);
    }
}
```
In the above code, `Debug.WriteLine()` method prints the debug messages which will be displayed in the output window of the IDE (like Visual Studio) while debugging. The output will look something like this:

```
Starting program
The number is 5
```
It's important to know that these debug messages are only included in the Debug build and not in the Release build of your application.

## Deep Dive

Originally, programmers would resort to creating log files or just printing out text directly on the console to understand the internal workings of their software. The `Debug` class was introduced in .NET framework to provide a standardized and convenient way to output debug information.

While `Debug.WriteLine()` is most commonly used, there are alternatives in C#. For instance, `Trace.WriteLine()` behaves similarly but is included both in Debug and Release builds, which can be useful when debugging production issues.

Under the hood, both `Debug.WriteLine()` and `Trace.WriteLine()` work by pushing messages to attached trace listeners, the default one being the Output Window in Visual Studio. You can implement and attach your own trace listeners if you want more control over where your debug output goes.

## See Also

[Basics of Debugging](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-absolute-beginners?view=vs-2019) 

[Trace class in C#](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.trace?view=net-5.0) 

[Debug and Trace classes](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=net-5.0) 

[Implementing Trace Listener](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.tracelistener?view=net-5.0)
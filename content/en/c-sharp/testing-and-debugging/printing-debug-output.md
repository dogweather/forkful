---
date: 2024-01-20 17:52:12.088875-07:00
description: "How to: Straightforward stuff: use `Console.WriteLine()` to print to\
  \ the output console. For debugging purposes specifically, `Debug.WriteLine()` can\
  \ be\u2026"
lastmod: '2024-03-13T22:45:00.091509-06:00'
model: gpt-4-1106-preview
summary: Straightforward stuff.
title: Printing debug output
weight: 33
---

## How to:
Straightforward stuff: use `Console.WriteLine()` to print to the output console. For debugging purposes specifically, `Debug.WriteLine()` can be your go-to, provided you have `System.Diagnostics` in your using directives. If you're targeting a UI application, `Trace.WriteLine()` could be the tool for the job since it allows listeners to capture the output.

```C#
using System;
using System.Diagnostics;

public class DebugExample
{
    public static void Main()
    {
        int magicNumber = 42;
        Console.WriteLine("Hello, folks! Let's debug.");
        Debug.WriteLine($"The magic number is: {magicNumber}");

        // Pretend we have a conditional here
        Trace.WriteLine("We're in the matrix!");
    }
}
```

The console output will look like:
```
Hello, folks! Let's debug.
```

The debug output, visible in your IDE's debug output window or listener, will be:
```
The magic number is: 42
We're in the matrix!
```

## Deep Dive
Let's time travel. When C# was a newbie, people debugged with message boxes—imagine clicking 'OK' a hundred times. But tools evolve. The 'Console.WriteLine()' method is a trusty, quick way to print outputs, best used in console apps. However, when you've moved on from console apps to developing Windows Forms or WPF apps, for instance, 'Debug.WriteLine()' and 'Trace.WriteLine()' from the `System.Diagnostics` namespace become more appealing.

'Debug.Writeline()' only outputs when the build is in Debug mode; it's silent in Release mode. This behavior makes it neat for temporary debug prints you don't worry about cleaning up later. On the other hand, 'Trace.WriteLine()' can be enabled for both Debug and Release builds, which can help with tracing issues after deployment.

It's worth noting that `Debug` and `Trace` calls can be peppered throughout your code, and you can control their output using Listeners, without needing to recompile every time you change where the output goes. Cool, right?

## See Also
For more giggles and knowledge nuggets, check out these links:
- Microsoft's official documentation on `Debug`: [Debug Class (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
- Microsoft's official documentation on `Trace`: [Trace Class (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.trace)
- A deep dive into listeners and trace sources: [Trace Listeners](https://docs.microsoft.com/en-us/dotnet/framework/debug-trace-profile/trace-listeners)

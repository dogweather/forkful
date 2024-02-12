---
title:                "Виведення налагоджувальної інформації"
aliases:
- uk/c-sharp/printing-debug-output.md
date:                  2024-01-20T17:52:18.834576-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Printing debug output – це як брати нотатки в процесі письма коду, щоб бачити, що відбувається в середині програми. Програмісти роблять це, щоб легше виявляти та вирішувати помилки.

## How to: (Як це зробити:)
```C#
using System;

class DebugExample
{
    static void Main()
    {
        int a = 5;
        int b = 10;
        int sum = a + b;
        
        // Print to console
        Console.WriteLine("Debug Output: The sum is " + sum);
    }
}
```
Sample output:
```
Debug Output: The sum is 15
```

## Deep Dive (Поглиблений Розбір)
In the early days, debugging was often done using print statements. These statements let you peek inside the flow of your program at specific checkpoints. C# provides `Console.WriteLine()` for printing output to the console, which makes it a basic but powerful tool when debugging.

Alternatives to `Console.WriteLine()` include using advanced debugging tools like Visual Studio Debugger, which allows setting breakpoints, watching variables, inspecting the call stack, etc. Also, there's `Debug.WriteLine()` for output that only appears during debugging and `Trace.WriteLine()` for when you need detailed execution traces in production. These write to listeners and can be configured in the `app.config` file.

Implementation wise, writing debug output can also be managed by various logging frameworks like NLog or log4net. These frameworks offer more control and can direct your logs to different destinations like files, databases, or external monitoring services.

## See Also (Дивіться Також)
- [Microsoft Docs: Debugging in Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [Microsoft Docs: Trace Listeners](https://docs.microsoft.com/en-us/dotnet/framework/debug-trace-profile/trace-listeners)
- [NLog](https://nlog-project.org/)
- [log4net](http://logging.apache.org/log4net/)

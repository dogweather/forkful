---
title:                "Drukowanie komunikatów debugowania"
aliases:
- /pl/c-sharp/printing-debug-output/
date:                  2024-01-20T17:52:31.645237-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Debugowanie to proces śledzenia, co się dzieje w programie. Programiści drukują informacje debugowania, żeby łatwiej znaleźć i naprawić błędy.

## How to (Jak to zrobić):
```C#
using System;

class Program
{
    static void Main()
    {
        // Prosty przykład wypisania tekstu na konsoli
        Console.WriteLine("Hej, tu info debugowe.");

        // Formatowanie danych wyjściowych
        int zIndex = 42;
        Console.WriteLine($"Wartość zIndex: {zIndex}");

        // Przy użyciu Debug.WriteLine (pamiętaj o dodaniu 'using System.Diagnostics;')
        Debug.WriteLine("To pokaże się tylko podczas debugowania.");
    }
}
```
Oczekiwane wyjście:

```
Hej, tu info debugowe.
Wartość zIndex: 42
```
W Debug output:

```
To pokaże się tylko podczas debugowania.
```

## Deep Dive (Pogłębiona wiedza):
Historia mechanizmów debugowania sięga początków programowania. Kiedyś debuggerów prawie nie było, więc wypisywanie informacji było podstawą. Alternatywy takie jak `Debug.WriteLine` i `Trace.WriteLine` w .NET pozwalają kontrolować, kiedy informacje są wypisywane, np. tylko przy debugowaniu, dzięki dyrektywom preprocesora jak `#if DEBUG`. Implementując debugowanie, programiści mogą używać również zewnętrznych narzędzi jak loggery, które oferują bardziej zaawansowane opcje jak filtrowanie czy formatowanie.

## See Also (Zobacz także):
- [Microsoft Docs: Debugowanie w Visual Studio](https://docs.microsoft.com/pl-pl/visualstudio/debugger/)
- [Microsoft Docs: Śledzenie i instrumentacja aplikacji](https://docs.microsoft.com/pl-pl/dotnet/framework/debug-trace-profile/tracing-and-instrumenting-applications)
- [Stack Overflow - Kiedy używać Console.WriteLine vs Debug.WriteLine?](https://stackoverflow.com/questions/3788605/if-debug-vs-conditionaldebug)

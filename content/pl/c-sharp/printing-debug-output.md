---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Wydrukowanie debugowania w C#

## Co i dlaczego?

Drukowanie debugowania to narzędzie programistyczne, które umożliwia wyświetlanie informacji o funkcjonowaniu kodu. Programiści używają go do szybkiej identyfikacji i naprawy błędów w kodzie.

## Jak to zrobić:

Oto przykład jak użyć wydruku debugowania w C#.

```C#
using System.Diagnostics;
 
public class TestClass
{
    public static void Main()
    {
        Debug.WriteLine("Uruchamianie testu");
        
        // tutaj jest część kodu do zdebugowania
        
        Debug.WriteLine("Zakończenie testu");
    }
}
```

Gdy uruchomisz ten program, zobaczysz tekst "Uruchamianie testu" i "Zakończenie testu" w oknie wyjścia.

## Zagłębianie się:

Drukowanie debugowania ma długą historię w świecie programowania i jest powszechnie stosowane niezależnie od języka kodowania. W C#, Debug.WriteLine() jest najpopularniejszym narzędziem. Alternatywami mogą być Console.WriteLine() lub Trace.WriteLine(). Zasadnicza różnica polega na tym, że Debug.WriteLine() nie jest uwzględniane w finalnej kompilacji Release, a Console.WriteLine() i Trace.WriteLine() są. 

## Zobacz również:

- [Debugger.Display attribute](https://docs.microsoft.com/pl-pl/dotnet/api/system.diagnostics.debuggerdisplayattribute?view=net-5.0)
- [Debugger in Visual Studio](https://docs.microsoft.com/pl-pl/visualstudio/debugger/?view=vs-2019)
- [Debugging Techniques and Tools](https://docs.microsoft.com/pl-pl/visualstudio/debugger/debugging-techniques-and-tools?view=vs-2019)

Oprócz tego polecam zagłębić się w zagadnienia debugowania i poznania różnych technik debugowania, które mogą przyspieszyć i ułatwić proces rozwijania oprogramowania.
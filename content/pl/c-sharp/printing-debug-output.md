---
title:                "Wydrukuj wyjście debugowania"
html_title:           "C#: Wydrukuj wyjście debugowania"
simple_title:         "Wydrukuj wyjście debugowania"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

Przykłady drukowania debugujących wyjść w C#

## Czym jest i dlaczego?

Drukowanie debugujących wyjść jest to proces polegający na wyświetlaniu informacji o działaniu programu podczas jego wykonywania w celu pomocy w debugowaniu i znajdowaniu błędów. Programiści robią to, aby lepiej zrozumieć jak ich kod działa i szybciej znaleźć potencjalne problemy.

## Jak to zrobić?

Dzięki poniższym przykładom w języku C# możesz zobaczyć jak łatwo jest dodać drukowanie debugujących wyjść do swojego kodu.

```C#
// Przykładowa funkcja, która drukuje debugujące wyjście
void PrintDebugOutput(string message)
{
    Console.WriteLine("DEBUG: " + message);
}

// Wywołanie funkcji w kodzie
int result = 5 + 10;
PrintDebugOutput("Wynik wynosi: " + result);
```

Wynik:
```
DEBUG: Wynik wynosi: 15
```

## Głębsza analiza

Drukowanie debugujących wyjść jest popularną techniką używaną przez programistów od lat. Pierwotnie wykorzystywano do tego celu pisanie na konsoli, jednak z czasem powstały narzędzia i biblioteki, które ułatwiają i rozwijają możliwości drukowania debugujących wyjść. Alternatywami dla drukowania debugujących wyjść mogą być również debuggery i profiling tools. W C# istnieje również wiele wbudowanych funkcji i metod do generowania debugujących wyjść, takich jak Debug.WriteLine() czy Trace.WriteLine().

W celu zwiększenia czytelności i wydajności kodu, zaleca się używanie warunków sprawdzających czy tryb debugowania jest włączony przed wywołaniem funkcji do drukowania debugujących wyjść. Można to zrobić za pomocą preprocesora #if DEBUG.

## Zobacz również

- Dokumentacja C# o drukowaniu debugujących wyjść: https://docs.microsoft.com/en-us/dotnet/core/diagnostics/logging
- Przewodnik po debugowaniu w C#: https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour
- Wprowadzenie do profilowania aplikacji w C#: https://docs.microsoft.com/en-us/visualstudio/profiling/beginners-guide-to-performance-profiling
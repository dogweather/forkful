---
title:                "Drukowanie wyjścia debugowania"
html_title:           "C#: Drukowanie wyjścia debugowania"
simple_title:         "Drukowanie wyjścia debugowania"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie wyjścia debugowania jest nieodłączną częścią programowania w języku C#, ponieważ pozwala na łatwiejsze znalezienie błędów i rozwiązywanie problemów w kodzie. Jest to niezbędne dla każdego programisty, niezależnie od poziomu doświadczenia.

## Jak to zrobić

W celu drukowania wyjścia debugowania w języku C#, możemy użyć funkcji ```Console.WriteLine()``` lub ```Debug.WriteLine()```. Oba metody działają podobnie, ale ```Debug.WriteLine()``` jest zalecane, ponieważ jest szybsze i ma większe możliwości formatowania wyjścia.

Przykładowy kod drukujący wyjście debugowania:

```C#
int number = 10;
string name = "Poland";

Console.WriteLine("Liczba: " + number);
Debug.WriteLine("Kraj: " + name);
```

*Wynik:*

```bash
Liczba: 10
Kraj: Poland
```

Dodatkowo, możemy także ustawić poziom wyjścia debugowania, korzystając z ```Debug.Listeners``` i ```Debug.Listeners.Clear()```. Jest to przydatne w przypadku, gdy chcemy kontrolować wyjścia debugowania w zależności od poziomu naszego programu.

## Głębszy wgląd

Drukowanie wyjścia debugowania może również być używane do monitorowania działania aplikacji i wychwytywania wyjątków. Korzystając z wyrażeń warunkowych, możemy ustawić warunek, który będzie drukował wyjście debugowania tylko wtedy, gdy warunek zostanie spełniony.

Możemy także korzystać z różnych poziomów wyjścia debugowania, takich jak ```Trace```, ```Info```, ```Warning``` i ```Error```, które pomagają nam w wyświetlaniu różnych rodzajów informacji w zależności od potrzeb.

## Zobacz także

- [Dokumentacja Microsoft na temat drukowania wyjścia debugowania w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.diagnostics.debug?view=net-5.0)
- [Przewodnik dla początkujących w języku C#](https://www.tutorialspoint.com/csharp/index.htm)
- [Przydatne narzędzia do debugowania w języku C#](https://www.c-sharpcorner.com/article/the-top-10-debugging-tools-in-c-sharp/)
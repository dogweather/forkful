---
title:                "Pisanie do standardowego błędu"
html_title:           "C#: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Writing to standard error jest procesem, w którym programista wysyła błędy, ostrzeżenia lub inne komunikaty do standardowego strumienia błędów, zamiast do standardowego strumienia wyjścia. Programiści często używają standardowego strumienia błędów do informowania użytkowników o błędach w swoim kodzie lub do debugowania programów.

## Jak to zrobić:
```C#
Console.Error.WriteLine("To jest komunikat wysłany do standardowego strumienia błędów.");
```

```
Output: To jest komunikat wysłany do standardowego strumienia błędów.
```

## Głębsze zanurzenie:
Wysyłanie komunikatów do standardowego strumienia błędów jest ważnym aspektem programowania, ponieważ pozwala na poprawne zarządzanie komunikacją między programem a użytkownikiem. Alternatywą dla standardowego strumienia błędów jest standardowy strumień wyjścia, który służy do wyświetlania oczekiwanych wyników działania programu. 

Implementacja standardowego strumienia błędów w języku C# jest prostym procesem, ponieważ wystarczy użyć metody Error z klasy Console. Standardowy strumień błędów jest również używany w celu zapisywania logów lub informacji diagnostycznych w trakcie działania programu.

## Zobacz również:
- Dokumentacja C# dotycząca standardowego strumienia błędów: [https://docs.microsoft.com/en-us/dotnet/api/system.console.error](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- Video tutorial na temat standardowego strumienia błędów w języku C#: [https://www.youtube.com/watch?v=PT7qH5PTCb0](https://www.youtube.com/watch?v=PT7qH5PTCb0)
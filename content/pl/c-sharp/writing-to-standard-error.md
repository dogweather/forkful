---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapis do standardowego błędu (stderr) pozwala oddzielić komunikaty o błędach od normalnego wyjścia programu (stdout). Programiści to robią, by łatwiej zarządzać błędami i umożliwić ich przekierowanie lub niezależne logowanie.

## Jak to zrobić:
```C#
using System;

class Program
{
    static void Main()
    {
        Console.Error.WriteLine("To jest komunikat błędu."); // zapis do stderr
        Console.WriteLine("To jest normalne wyjście programu."); // zapis do stdout
    }
}
```
Przykładowe wyjście:
```
> dotnet run 2>err.log
To jest normalne wyjście programu.
> cat err.log
To jest komunikat błędu.
```

## Głębsze zrozumienie:
Początkowo, w systemach uniksowych, stderr służył do informowania użytkownika o błędach bez przerywania wyjścia procesu. Alternatywami dla stderr są logi, wyjątki czy zdarzenia. W C# stderr jest zaimplementowany jako `Console.Error`, strumień który możesz użyć podobnie jak standardowe wyjście (`Console.WriteLine`), ale jest domyślnie przekierowany do ekranu błędów w terminalu.

## Zobacz również:
- [Dokumentacja Microsoft o .NET Core Console.Error Property](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [Wikipedia o standard streams](https://en.wikipedia.org/wiki/Standard_streams)
- [Stack Overflow: When to use Console.WriteLine vs Console.Error.WriteLine?](https://stackoverflow.com/questions/178456/when-to-use-console-writeline-vs-console-error-writeline)

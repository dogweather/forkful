---
title:    "C#: Odczytywanie argumentów wiersza poleceń"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Odczytywanie argumentów wiersza poleceń jest niezbędnym elementem wielu aplikacji i skryptów. Jest to przydatna umiejętność, która pozwala na interaktywną pracę z programem, dostosowanie jego działania oraz przekazywanie informacji od użytkownika. W tym artykule dowiesz się, jak odczytywać argumenty wiersza poleceń w języku C#, co pozwoli Ci na jeszcze większą swobodę w tworzeniu oprogramowania.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń w C#, musisz wykorzystać klasę `Environment` oraz metodę `GetCommandLineArgs()`. Poniższy przykład kodu pokazuje, jak wygląda to w praktyce:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        // odczytaj argumenty wiersza poleceń
        string[] arguments = Environment.GetCommandLineArgs();

        // wyświetl listę argumentów
        Console.WriteLine("Liczba argumentów: " + arguments.Length);
        foreach (string arg in arguments)
        {
            Console.WriteLine(arg);
        }
    }
}
```
**Wyjście:**

`Liczba argumentów: 3`  
`C:\App\my_app.exe`  
`arg1`  
`arg2`

Jak widać, metoda `GetCommandLineArgs()` zwraca tablicę zawierającą wszystkie argumenty przekazane do programu. Należy pamiętać o tym, że pierwszy element tej tablicy zawsze będzie zawierał ścieżkę do programu.

Jeśli chcesz przekazać do programu argumenty wiersza poleceń, możesz to zrobić poprzez podanie ich po spacji po nazwie pliku wykonywalnego w wierszu poleceń. Na przykład: `my_app.exe arg1 arg2`.

## Dogłębna analiza

Metoda `GetCommandLineArgs()` jest bardzo przydatna, ale nie jest jedynym sposobem na odczytanie argumentów wiersza poleceń w C#. Istnieją również inne metody, takie jak wykorzystanie klasy `Console` i jej metody `ReadLine()`, która pozwala użytkownikowi na wprowadzenie argumentów po uruchomieniu programu.

## Zobacz także

Więcej informacji na temat programowania w C# znajdziesz w następujących artykułach:

- [Tworzenie aplikacji konsolowych w C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/main-and-command-args/)
- [Obsługa argumentów wiersza poleceń w C#](https://www.tutorialspoint.com/csharp/csharp_command_line_arguments.htm)
- [Różne sposoby odczytywania argumentów wiersza poleceń w C#](https://www.c-sharpcorner.com/article/ways-to-pass-command-line-arguments-to-a-c-sharp-program/)
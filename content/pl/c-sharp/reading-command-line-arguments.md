---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Praca z argumentami wiersza polecenia w C#

## Co i dlaczego?
Argumenty wiersza polecenia to parametry przekazywane do programu podczas jego uruchomienia. Pozwalają użytkownikowi na wpływanie na funkcję programu bez zmieniania kodu.

## Jak to zrobić:
Najprostszą drogą do odczytywania argumentów wiersza polecenia w C# jest użycie tablicy stringów jako parametru dla metody `Main()`. 

```C#
static void Main(string[] args)
{
    foreach (string arg in args)
    {
        Console.WriteLine(arg);
    }
}
```

Gdy uruchomisz program z linii poleceń, np. `program.exe arg1 arg2`, wydrukuje:

```
arg1
arg2
```

## Zagłębianie się
Argumenty wiersza polecenia były podstawą sterowania najwcześniejszymi programami komputerowymi. W przeciwieństwie do graficznego interfejsu użytkownika (GUI), wiersz poleceń umożliwia precyzyjne i zautomatyzowane sterowanie programami. 

Zamiast metody `Main()`, alternatywnie możemy użyć klasy `Environment` i jej właściwości `GetCommandLineArgs()`. Ta metoda jest rzadziej używana, ale daje dostęp do pełnej listy argumentów, w tym nazwy pliku wykonywalnego.

Szczegóły implementacji zależą od środowiska wykonawczego .NET, ale argumenty są zazwyczaj przekazywane do procesu przez system operacyjny.

## Zobacz także
Gdy już opanujesz argumenty wiersza polecenia, możesz chcieć dowiedzieć się więcej o innych aspektach C# i .NET. 

Dokumentacja Microsoft na temat argumentów wiersza polecenia:
[https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/main-and-command-args/](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/main-and-command-args/)

Interactive C# Tutorial: [https://www.learn-c-sharp.com/](https://www.learn-c-sharp.com/)
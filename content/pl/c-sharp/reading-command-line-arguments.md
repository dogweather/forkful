---
title:                "C#: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią wielu dziedzin naszego życia. Wiele osób zainteresowanych jest nauką języków programowania, ale niektórzy z pewnością zastanawiają się, jakie są najważniejsze aspekty, którymi powinni się zająć. Jednym z nich jest odczytywanie argumentów wiersza poleceń, co jest kluczowym elementem nie tylko dla systemów operacyjnych, ale również dla wielu aplikacji. W tym blogu opowiemy o tym, dlaczego warto zgłębić ten temat i jak to zrobić.

## Jak to zrobić

Odczytywanie argumentów wiersza poleceń jest możliwe dzięki wykorzystaniu metody `GetCommandLineArgs()` w języku C#. Przeanalizujemy przykład poniżej, aby lepiej zrozumieć, jak to działa:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Podane argumenty: ");
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine(args[i]);
        }
    }
}
```
Wywołując ten kod z określonymi argumentami wiersza poleceń, otrzymamy następujący wynik:

```
$ C:\Users\User\Desktop\Program.exe argument1 argument2
Podane argumenty:
argument1
argument2
```

W powyższym przykładzie wykorzystujemy pętlę `for`, aby wyświetlić wszystkie podane argumenty. Zauważmy również, że pierwszym argumentem jest nazwa pliku wykonywalnego, dlatego w pętli zaczynamy od indeksu 1.

## Dogłębne wody

Warto wiedzieć, że oprócz podstawowej metody `GetCommandLineArgs()`, istnieją również inne sposoby na odczytywanie argumentów wiersza poleceń w C#. Jednym z nich jest wykorzystanie przestrzeni nazw `System.Environment`, która oferuje różne metody takie jak `GetCommandLine()` lub `GetCommandLineArgs()`. Można również korzystać z biblioteki `CommandLineParser`, która ułatwia zarządzanie i przetwarzanie argumentów wiersza poleceń. 

## Zobacz również

- [Dokumentacja Microsoft o odczytywaniu argumentów wiersza poleceń w C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [Biblioteka CommandLineParser](https://commandlineparser.codeplex.com/)
- [Poradnik wideo NTuTech o odczytywaniu argumentów wiersza poleceń](https://www.youtube.com/watch?v=I-puz0iF6QA)
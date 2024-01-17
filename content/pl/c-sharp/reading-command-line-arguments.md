---
title:                "Odczytywanie argumentów z linii poleceń"
html_title:           "C#: Odczytywanie argumentów z linii poleceń"
simple_title:         "Odczytywanie argumentów z linii poleceń"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Odczytywanie argumentów z wiersza poleceń to częsty aspekt programowania, który pozwala programom działać w zależności od różnych warunków. Programiści używają tej techniki, ponieważ pozwala im na dostosowywanie działania programu bez konieczności zmieniania kodu źródłowego.

# Jak to zrobić:

~~~C#
/* Przykładowy kod w języku C# pokazujący jak odczytać argumenty z wiersza poleceń */

using System;

class Program
{
    static void Main(string[] args)
    {
        // Jeśli wprowadzone zostały argumenty
        if (args.Length > 0)
        {
            // Wyświetl pierwszy argument
            Console.WriteLine("Pierwszy argument: " + args[0]);
        }
        else
        {
            // Wyświetl informację o braku argumentów
            Console.WriteLine("Brak wprowadzonych argumentów.");
        }
    }
}

/* Przykładowe wywołanie programu z argumentami "argument1 argument2" spowoduje wyświetlenie tekstu "Pierwszy argument: argument1" */

~~~

# Wprowadzenie do tematu:

Odczytywanie argumentów z wiersza poleceń nie jest nową techniką i powszechnie używana jest w wielu językach programowania. Alternatywą dla ręcznego odczytywania argumentów jest użycie gotowej biblioteki lub frameworka. W języku C# istnieje wiele takich rozwiązań, na przykład biblioteka "Command Line Parser" lub framework "Microsoft.Extensions.CommandLineUtils".

# Zobacz też:

- Dokumentacja dotycząca odczytywania argumentów z wiersza parancov
- Poradnik dla programistów dotyczący wykorzystania biblioteki "Command Line Parser" w języku C#
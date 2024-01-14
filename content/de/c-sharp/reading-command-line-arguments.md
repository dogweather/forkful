---
title:                "C#: Lesen von Befehlszeilenargumenten"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein wichtiger Teil der Programmierung in C#. Mit der Kenntnis und Verwendung von Befehlszeilenargumenten können Sie Ihrem Programm die Fähigkeit geben, spezifische Daten oder Optionen zu erhalten, die Sie bei der Ausführung des Codes anpassen möchten. Dies kann die Effizienz beim Testen und Debuggen verbessern und die Anwendung insgesamt flexibler und benutzerfreundlicher machen.

## So geht's

Das Lesen von Befehlszeilenargumenten ist in C# relativ einfach. Dazu müssen Sie zunächst das `System`-Namespace und den `Main()`-Abschnitt Ihres Codes nutzen. In diesem Abschnitt können Sie mit der statischen `Environment`-Klasse und der `GetCommandLineArgs()`-Methode auf die Befehlszeilenargumente zugreifen. Dies kann wie folgt aussehen:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        // Code, der Befehlszeilenargumente liest und ausgibt
        string[] arguments = Environment.GetCommandLineArgs();

        Console.WriteLine("Die übergebenen Befehlszeilenargumente sind:");
        foreach (string argument in arguments)
        {
            Console.WriteLine(argument);
        }
    }
}
```

Wenn Sie diesen Code ausführen, erhalten Sie eine Ausgabe ähnlich der folgenden:

```
Die übergebenen Befehlszeilenargumente sind:
C:\Program Files\MyProgram.exe
Argument1
Argument2
Argument3
```

Als nächstes können Sie die `args`-Parameterübergabe in der `Main()`-Methode nutzen, um gezielt auf bestimmte Befehlszeilenargumente zuzugreifen. Sie können beispielsweise eine Bedingung einfügen, die überprüft, ob das erste Befehlszeilenargument dem gewünschten Format entspricht, und dann basierend darauf eine Aktion ausführen. Hier ein Beispiel:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        // Code, der Befehlszeilenargumente liest und verarbeitet
        string[] arguments = Environment.GetCommandLineArgs();

        // Überprüfung auf ein bestimmtes Befehlszeilenargument
        if (arguments[0].EndsWith(".txt"))
        {
            // Durchsucht das übergebene Textdokument nach bestimmten Wörtern
            string[] textLines = System.IO.File.ReadAllLines(arguments[0]);

            string searchString = "Lorem ipsum";
            int counter = 0;

            foreach (string line in textLines)
            {
                if (line.Contains(searchString))
                {
                    counter++;
                }
            }

            Console.WriteLine($"Das Wort \"{searchString}\" kommt {counter} mal in dem Textdokument vor.");
        }
    }
}
```

In diesem Beispiel wird überprüft, ob das erste Befehlszeilenargument mit ".txt" endet (was darauf hinweist, dass hier eine Textdatei übergeben wurde). Wenn dies der Fall ist, wird das Programm die Datei öffnen und nach dem Suchbegriff "Lorem ipsum" suchen. Die Ausgabe könnte dann beispielsweise lauten: "Das Wort "Lorem ipsum" kommt 5 mal in dem Textdokument vor."

## Tiefer Einblick

Während das Lesen von Befehlszeilenargumenten in C# relativ einfach ist, gibt es doch einige Besonderheiten, auf die Sie achten sollten. Zum Beispiel werden Befehlszeilenargumente, die in Anführungszeichen gesetzt sind (z.B. `MyProgram.exe "Argument 1" "Argument 2"`), als einzelne Argumente behandelt, auch wenn sie Leerzeichen enthalten. Um diese Argumente als eine Einheit zu lesen, müssen Sie zusätzlich den `String.Join()`-Befehl verwenden. Hier ein Beispiel:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        string allArguments = String.Join(" ", args);
        Console.Write($"Alle Befehlszeilenargumente in einem String: {allArguments}");
    }
}
```

In diesem Beispiel werden alle Befehlszeilenargumente in einer Zeile ausgegeben, inklusive der Leerzeichen zwischen den Argumenten. Die Ausgabe könnte dann beispielsweise lauten: "Alle Befehlszeilenargumente in einem String: Argument1 Argument
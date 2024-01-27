---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Reguläre Ausdrücke ermöglichen Dir, Muster in Strings zu finden und zu manipulieren. Programmierer nutzen sie für valide Eingabeprüfungen, Suchen und Ersetzen von Text sowie für komplexe Textanalysen.

## How to:
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        // Ein einfaches Matching-Beispiel
        string pattern = @"\d+";
        string input = "Es gibt 123 Äpfel und 456 Birnen.";
        MatchCollection matches = Regex.Matches(input, pattern);

        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }
        // Ausgabe:
        // 123
        // 456

        // Beispiel für Ersetzungen
        string replacePattern = @"\bApfel\b";
        string result = Regex.Replace(input, replacePattern, "Orange");
        Console.WriteLine(result);
        // Ausgabe:
        // Es gibt 123 Orangen und 456 Birnen.
    }
}
```

## Deep Dive
Reguläre Ausdrücke gibt es seit den 1950er Jahren, entwickelt von Stephen Cole Kleene. Abseits der C# `System.Text.RegularExpressions`-Klasse können auch andere Bibliotheken wie `PCRE` oder Sprachfeatures wie LINQ für manche Aufgaben eingesetzt werden. Die Implementierung in .NET basiert auf einem NFA (Nondeterministic Finite Automaton), was Performance-Implikationen hat - besonders bei "backtracking".

## See Also
- [Microsoft Docs zu Regular Expressions](https://docs.microsoft.com/de-de/dotnet/standard/base-types/regular-expressions)
- [Regexr: Online-Tool zum Testen von regulären Ausdrücken](https://regexr.com/)
- [Regular Expression Library: Eine Sammlung von fertigen Ausdrücken](http://www.regexlib.com/)

---
title:                "Använda reguljära uttryck"
html_title:           "Gleam: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad och varför?

Regular expressions (RegEx) är en kraftfull teknik för att matcha och arbeta med textmönster. Programmers använder det på grund av dess flexibilitet att matcha, hitta, manipulera och splittra strängar baserade på bestämda mönster.

## Hur till:

I C# används System.Text.RegularExpressions namespace för att jobba med RegEx. MCS-standarden är använd för syntaxen. Här är ett exempel:

```C#
using System;
using System.Text.RegularExpressions;

class Example {
    static void Main() {
        Regex rgx = new Regex(@"\d+");    // matchar alla sekvenser av en eller flera siffror
        string sentence = "13 äpplen, 47 päron och 0 vindruvor.";
        
        MatchCollection matches = rgx.Matches(sentence);
        foreach (Match match in matches) {
            Console.WriteLine("Matchat nummer: " + match.Value);
        }
    }
}
```

Output:

```
Matchat nummer: 13
Matchat nummer: 47
Matchat nummer: 0
```

## Fördjupning

RegEx härstammar från 1950-talets teoretiska datavetenskap, men har anammats i många programmeringsspråk inklusive C#. Det finns flera alternativ till RegEx som LINQ och String Methods, men inget är lika dynamiskt. Oftast implementeras RegEx i C# genom metoder som `Match()`, `Matches()`, `Replace()` och `Split()`, vilka returnerar anpassade resultat baserade på tillämpat mönster.

## Se även

1. Microsoft's officiella dokumentation: [Regular Expressions (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/regular-expressions)
2. Video tutorial: [C# Regular Expressions](https://www.youtube.com/watch?v=OwDt-klLgE0)
3. Online regex testare: [Regex Storm](https://regexstorm.net/tester)
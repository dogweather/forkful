---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hitta Längden på en Sträng i C#: En Snabbguide för de Nyfikna Programmerare
_Dagens C# programmeringstips är att hitta längden på en sträng. Vi ska dyka rakt in!_

## Vad & Varför?

Att hitta längden på en sträng innebär att räkna antalet tecken i den angivna strängen. Programmerare gör detta för datahantering och manipulation.

## Hur man gör:

Här är ett grundläggande exempel i C#.

```C#
string minStrang = "Hej Sverige!";
int langd = minStrang.Length;
Console.WriteLine("Längden på strängen är: " + langd);
```

När du kör detta program, får du detta som output:

```
Längden på strängen är: 13
```

## Djupdykning:

Hitta längden på en sträng är ett gammalt behov, ända tillbaka till de tidiga dagarna av kodning. I C#, får du strängens längd via 'Length' egenskapen i 'String Class'.

Det finns alternativ till ’Length’, som LINQ-metoden ’Count()’, men man ska observera att ’Length’ är snabbare eftersom det är en egenskap inte en metod. 

Längden på en sträng i C# beräknas genom att räkna antalet Unicode-tecken. Observera att ett 'Char' i .NET faktiskt kan representera mindre än ett helt tecken - de kan vara delar av ett tecken, för att hantera komplexa tecken som emojis.

## Se även:

* [Stack Overflow Diskussion om Stränglängd vs Count()](https://stackoverflow.com/questions/94591/what-is-the-difference-between-length-and-count) 

Glöm inte att pröva dig fram och experimentera med det du har lärt dig. Lycka till med din programmeringsresa!
---
title:                "Sammanfogning av strängar"
html_title:           "C#: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att sammanslå strängar är en vanlig operation inom programmering där strängar (text) kombineras till en enda sträng. Detta kan användas för att skapa mer dynamiska och anpassade meddelanden och uttryck inom en applikation.

Det är vanligt att programmerare använder sammanslagning av strängar för att skapa dynamiska och anpassade meddelanden till användare, visa data i en läsbar form eller manipulera data för att uppfylla specifika behov.

# Hur man gör:
Du kan använda funktionen "string.Concat" för att sammanslå två eller flera strängar i C#. Du kan också använda "+" operatorn för att sammanslå strängar. Exempelvis:

```C#
string s1 = "Detta är";
string s2 = "en sträng";
string resultat = string.Concat(s1, " ", s2);
Console.WriteLine(resultat);
```
Output: Detta är en sträng.

# Djupdykning:
Sammanslagning av strängar har funnits sedan de tidiga dagarna av programmering. Innan det fanns funktioner som "string.Concat", användes operatorsymbolen "&" för att sammanslå strängar i C#. Det finns också andra sätt att sammanslå strängar, som att använda String.Format() funktionen eller StringBuilder klassen i C#.

När det kommer till prestanda, är det bäst att använda StringBuilder klassen för att sammanslå stora mängder av strängar, eftersom den används för att bygga en sträng steg för steg istället för att skapa en ny sträng varje gång som Concat-funktionen gör.

# Se även:
- [MSDN Documentation on String.Concat Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat)
- [MSDN Documentation on String.Format Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.format)
- [MSDN Documentation on String Builder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)
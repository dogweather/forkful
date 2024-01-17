---
title:                "Jämföra två datum"
html_title:           "C#: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum är när man tittar på två specifika datum och jämför dem för att se vilket som är tidigare eller senare. Programmerare gör detta för att kunna hantera datum på ett effektivt sätt och få korrekt data från sina program.

## Så här:
Det enklaste sättet att jämföra två datum är att använda kommandot ".Compare()" i C#.

```C#
DateTime datum1 = new DateTime(2020, 01, 01);
DateTime datum2 = new DateTime(2021, 01, 01);

int resultat = datum1.Compare(datum2);
Console.WriteLine(resultat);
```

Output:

```
-1
```
I det här exemplet jämförs datum1 med datum2. Eftersom datum1 är tidigare än datum2, blir resultatet -1.

## Djupdykning:
Jämförelse av datum är en viktig del av programmering eftersom det möjliggör för oss att hantera tid och datum på ett korrekt sätt. Det finns flera alternativ för att jämföra datum, såsom "=="-operatorn eller använda metoden ".Equals()". Det är viktigt att vara noga med hur man jämför datum eftersom olika format och parametrar kan påverka resultatet.

## Se även:
- [C# DateTime.Compare() Metod (Microsoft Documentation)](https://docs.microsoft.com/sv-se/dotnet/api/system.datetime.compare)
- [C# Comparison of Dates (C# Corner Article)](https://www.c-sharpcorner.com/article/comparison-of-dates-in-c-sharp/)
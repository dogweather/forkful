---
title:                "Extrahera substrängar"
html_title:           "C#: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att extrahera delsträngar är när man plockar ut en del av en större sträng. Detta är användbart för att hantera och manipulera data, som att separera en användares namn eller e-postadress från en större text. Programmers använder ofta detta för att bearbeta och organisera data på ett effektivt sätt.

## Såhär gör du:

```C#
// Skapa en sträng
string text = "Hej alla, jag heter Maria";

// Extrahera delsträngen "Maria" från texten
string delsträng = text.Substring(20, 5);
// 20 är startpositionen och 5 är längden på delsträngen

// Skriv ut delsträngen
Console.WriteLine(delsträng);
// Output: Maria
```

## Djupdykning:

Historiskt sett har substring extraction varit en konceptuell del av programmering sedan tidiga språk som C. Det finns också alternativ till att använda substrings, såsom metoder som `Split()` för att dela upp en sträng baserat på ett angivet tecken eller mönster. Implementationen av substring extraction i C# är mycket effektiv och inbyggd i språket för att göra det enkelt för programmerare att använda.

## Se även:

- [Documentation for Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring) 
- [C# String Manipulation Guide](https://www.tutorialspoint.com/csharp/csharp_string_manipulation.htm)
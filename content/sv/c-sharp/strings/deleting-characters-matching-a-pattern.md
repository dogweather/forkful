---
aliases:
- /sv/c-sharp/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:41:51.255303-07:00
description: "Att radera tecken som matchar ett m\xF6nster inneb\xE4r att man systematiskt\
  \ tar bort specifika tecken eller sekvenser fr\xE5n en str\xE4ng. Programmerare\
  \ g\xF6r detta\u2026"
lastmod: 2024-02-18 23:08:51.779142
model: gpt-4-1106-preview
summary: "Att radera tecken som matchar ett m\xF6nster inneb\xE4r att man systematiskt\
  \ tar bort specifika tecken eller sekvenser fr\xE5n en str\xE4ng. Programmerare\
  \ g\xF6r detta\u2026"
title: "Ta bort tecken som matchar ett m\xF6nster"
---

{{< edit_this_page >}}

## Vad & Varför?
Att radera tecken som matchar ett mönster innebär att man systematiskt tar bort specifika tecken eller sekvenser från en sträng. Programmerare gör detta för att rensa data, validera inmatning eller förbereda text för databehandling.

## Så här gör du:
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string myString = "Hej! Hur mår du idag 123?";
        string pattern = @"\d"; // Mönstret som matchar alla siffror

        string result = Regex.Replace(myString, pattern, "");
        Console.WriteLine(result); // Output: Hej! Hur mår du idag ?
    }
}
```
Koden använder `Regex.Replace` för att ersätta alla siffror (mönstret `\d`) i strängen med ingenting (ta bort dem).

## Djupdykning
Tillbaka i tiden var string manipulation mer manuell och ganska klumpig. Idag använder vi `Regex` (Regular Expressions) i C# för att effektivisera borttagningen av tecken som matchar intrikata mönster. Alternativt kan man iterera över en sträng och bygga en ny utan de oönskade tecknen, men det är oftast långsammare och mer kodkrävande. När det gäller implementation är `Regex` kraftfullt men kan vara långsamt för stora mängder text, så ibland kan enklare metoder som `String.Replace` eller `StringBuilder` vara att föredra för enkel substitution.

## Se även
- [Regular Expressions in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Regex.Replace Method](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)

---
date: 2024-01-20 17:57:32.110369-07:00
description: "Att s\xF6ka och ers\xE4tta text \xE4r grundl\xE4ggande: du letar efter\
  \ specifika teckenstr\xE4ngar och byter ut dem mot n\xE5got annat. Programmerare\
  \ g\xF6r det f\xF6r att snabbt\u2026"
lastmod: '2024-03-13T22:44:37.898277-06:00'
model: gpt-4-1106-preview
summary: "Att s\xF6ka och ers\xE4tta text \xE4r grundl\xE4ggande: du letar efter specifika\
  \ teckenstr\xE4ngar och byter ut dem mot n\xE5got annat."
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

## Hur gör man:
Låt oss dyka rakt in. Här är en snabb genomgång av hur man kan söka och ersätta text i C#:

```C#
using System;

class Program
{
    static void Main()
    {
        string originalText = "Hej, jag heter Codey. Jag är en kodningsentusiast.";
        string searchText = "kodningsentusiast";
        string replaceText = "programmerare";

        string updatedText = originalText.Replace(searchText, replaceText);

        Console.WriteLine(updatedText);
    }
}
```

Körs output:
```
Hej, jag heter Codey. Jag är en programmerare.
```

## Fördjupning
Låt oss gräva lite djupare. Söka och ersätta kom med tidiga textredigerare och utvecklingsmiljöer. I C#, `String.Replace()` är inbyggd i .NET och den funkar bra för enkla fall. För mer komplexa mönster använder vi `Regex.Replace()` från `System.Text.RegularExpressions` som hanterar reguljära uttryck.

Alternativ till `Replace()` inkluderar att manuellt loopa genom strängar och bygga nya med `StringBuilder`, vilket kan vara effektivare för stora datamängder eller då anpassad logik behövs.

Vad gäller implementering: `String.Replace()` skapar en ny sträng (eftersom strängar är oföränderliga i C#), vilket innebär en minnesöverhead om du jobbar med stora textmängder och gör många ersättningar.

## Se även
- Microsofts dokumentation för `String.Replace()`: [https://docs.microsoft.com/en-us/dotnet/api/system.string.replace](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- Intro till reguljära uttryck i C#: [https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- Fördjupning i `StringBuilder`: [https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)

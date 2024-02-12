---
title:                "Sökning och ersättning av text"
aliases:
- /sv/c-sharp/searching-and-replacing-text.md
date:                  2024-01-20T17:57:32.110369-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sökning och ersättning av text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text är grundläggande: du letar efter specifika teckensträngar och byter ut dem mot något annat. Programmerare gör det för att snabbt uppdatera kod, fixa fel eller ändra datan.

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

---
title:    "C#: Skapa en tillfällig fil"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför skapa en temporär fil?

Att skapa temporära filer kan vara användbart när du behöver tillfälligt lagra data eller information under körning av ditt program. Det kan också vara användbart för att undvika konflikter med befintliga filer eller för att minska risken för oavsiktlig dataändring.

## Hur man skapar en temporär fil i C#

För att skapa en temporär fil i C# kan du använda klassen `Path` och metoden `GetTempFileName()`. Se exemplet nedan:

```C#
using System.IO;

string tempFile = Path.GetTempFileName();
Console.WriteLine("Den temporära filen heter: " + tempFile);
```

Output: Den temporära filen heter: C:\TEMP\tmp1F8.tmp

Som du kan se i exemplet ovan så skapar metoden `GetTempFileName()` en unik filnamn baserat på systemets temporära mapp. Du kan sedan använda filnamnet för att läsa eller skriva data till din temporära fil.

## Djupdykning i skapandet av temporära filer

När du skapar en temporär fil, så skapas faktiskt en vanlig fil på din dator i systemets temporära mapp. Det unika filnamnet genereras av systemet och innehåller en unik kombination av bokstäver och siffror, vilket minskar risken för att namnet redan är upptaget.

Det är också viktigt att notera att en temporär fil endast är tillgänglig under körning av ditt program. När programmet avslutas kommer filen att tas bort automatiskt av systemet. Det är därför viktigt att inte förlita sig på en temporär fil som en permanent lagringslösning.

## Se även

- Microsoft Dokumentation om klassen `Path`: https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netcore-3.1
- Tutorial om att skapa temporära filer i C#: https://www.tutorialspoint.com/csharp/csharp_temporary_files.htm
- Stack Overflow fråga om skapande av temporära filer: https://stackoverflow.com/questions/1365407/how-to-create-a-temporary-file-in-c-sharp
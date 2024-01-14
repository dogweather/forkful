---
title:    "C#: Att läsa en textfil"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är ett grundläggande steg i programmering. Det låter dig hämta data från en extern källa och använda den i din kod. Det kan vara användbart för att skapa dynamiska program eller för att bearbeta stora mängder data.

## Hur man gör

För att läsa en textfil i C#, behöver du använda klassen `StreamReader` från `System.IO`-biblioteket. Först måste du ange sökvägen till textfilen och sedan öppna en ström som kan användas för att läsa filen. Detta kan göras genom att använda följande kod:

```C#
string filePath = @"C:\Users\UserName\Desktop\file.txt";
StreamReader file = new StreamReader(filePath);
```

När du har öppnat en `StreamReader`-instans, kan du använda metoden `ReadLine()` för att läsa en rad åt gången från filen. Detta kan vara till hjälp om du vill bearbeta filens innehåll rad för rad. Ett exempel på hur man kan läsa en textfil med radförteckningen i terminalen kan se ut såhär:

```C#
string line;
while ((line = file.ReadLine()) != null)
{
    Console.WriteLine(line);
}
```

Detta kommer att skriva ut varje rad i filen i terminalen. När du är klar med att läsa filen måste du stänga strömmen genom att använda metoden `Close()`:

```C#
file.Close();
```

## Deep Dive

Att läsa textfiler kan vara enkelt, men det finns flera alternativ och inställningar som du kan använda för att göra läsningen mer effektiv och anpassad efter dina behov. Här är några av de viktigaste aspekterna att tänka på när du läser en textfil:

- Encoding: Om din textfil är skriven i ett annat teckenuppsättning än standard, måste du ange rätt kodning när du öppnar strömmen för att säkerställa att innehållet visas korrekt.
- Separatortecken: Om din textfil använder ett speciellt tecken som separerar olika fält, till exempel ett kommatecken eller ett tabbtecken, kan du använda metoden `Split()` för att dela upp varje rad i en lista av strängar.
- Felhantering: När du läser en textfil är det viktigt att hantera eventuella fel som kan uppstå, till exempel om filen inte hittas eller om strömmen bryts mitt i läsningen.

Genom att läsa på om dessa och andra funktioner i `StreamReader`-klassen kan du optimera din kod för att läsa textfiler på ett effektivt sätt.

## Se även

- [C# StreamReader-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [C# File-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [Läsa och skriva i textfiler i C#](https://www.c-sharpcorner.com/article/reading-and-writing-the-text-files-in-c-sharp/)
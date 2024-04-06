---
date: 2024-01-20 17:40:33.541058-07:00
description: "How to: Att skapa tempor\xE4ra filer \xE4r ingen ny grej \u2013 programmerare\
  \ har gjort detta sedan urminnes tider f\xF6r att hantera utklipp, mellanlagring\
  \ av data\u2026"
lastmod: '2024-04-05T22:50:52.232484-06:00'
model: gpt-4-1106-preview
summary: "Att skapa tempor\xE4ra filer \xE4r ingen ny grej \u2013 programmerare har\
  \ gjort detta sedan urminnes tider f\xF6r att hantera utklipp, mellanlagring av\
  \ data eller sessioner."
title: "Skapa en tempor\xE4r fil"
weight: 21
---

## How to:
```C#
using System;
using System.IO;

class TemporaryFileExample
{
    static void Main()
    {
        // Skapa en temporär fil
        string tempFilePath = Path.GetTempFileName();
        
        // Visa sökväg till den temporära filen
        Console.WriteLine("Temp file created at: " + tempFilePath);
        
        // Använd filen här...
        
        // Glöm inte att rensa upp efteråt!
        File.Delete(tempFilePath);
        Console.WriteLine("Temp file deleted.");
    }
}
```
Exempelutdata:
```
Temp file created at: C:\Users\<Username>\AppData\Local\Temp\tmp1234.tmp
Temp file deleted.
```

## Deep Dive
Att skapa temporära filer är ingen ny grej – programmerare har gjort detta sedan urminnes tider för att hantera utklipp, mellanlagring av data eller sessioner. Före molntjänsternas tid var detta ett hett sätt att hålla en användarsession levande utan att överbelasta serverns huvudminne.

C# erbjuder klassen `Path` med metoden `GetTempFileName()` för att undvika namnkrockar och automatisera processen. Alternativet är att själv knacka ihop en filnamnsgenerator, men varför uppfinna hjulet igen?

En detalj att ha koll på är att `GetTempFileName()` faktiskt skapar filen på disken. Detta är både bra och dåligt: bra eftersom du sparar tid genom att inte behöva öppna en filström, och dåligt eftersom den tar upp fysiskt utrymme (om än minimalt) så snart metoden anropas.

## See Also
- [System.IO.Path.GetTempFileName Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)

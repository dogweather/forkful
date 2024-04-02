---
date: 2024-01-20 17:40:33.541058-07:00
description: "Skapa en tempor\xE4r fil \xE4r som att hugga ut en tillf\xE4llig hylla\
  \ i det digitala utrymmet att st\xE4lla saker p\xE5 medan du h\xE5ller p\xE5 med\
  \ dem. Programmerare g\xF6r\u2026"
lastmod: '2024-03-13T22:44:37.932787-06:00'
model: gpt-4-1106-preview
summary: "Skapa en tempor\xE4r fil \xE4r som att hugga ut en tillf\xE4llig hylla i\
  \ det digitala utrymmet att st\xE4lla saker p\xE5 medan du h\xE5ller p\xE5 med dem.\
  \ Programmerare g\xF6r\u2026"
title: "Skapa en tempor\xE4r fil"
weight: 21
---

## What & Why?
Skapa en temporär fil är som att hugga ut en tillfällig hylla i det digitala utrymmet att ställa saker på medan du håller på med dem. Programmerare gör detta för att handskas med data som behövs tillfälligt utan att kladda ner den permanenta lagringen.

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

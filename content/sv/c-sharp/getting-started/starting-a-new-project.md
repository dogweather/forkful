---
date: 2024-01-20 18:03:17.790824-07:00
description: "Hur man g\xF6r: F\xF6r att kickstarta ett nytt C#-projekt, anv\xE4nd\
  \ dotnet CLI (Command Line Interface). H\xE4r \xE4r grunderna."
lastmod: '2024-03-13T22:44:37.913949-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att kickstarta ett nytt C#-projekt, anv\xE4nd dotnet CLI (Command\
  \ Line Interface)."
title: "Att p\xE5b\xF6rja ett nytt projekt"
weight: 1
---

## Hur man gör:
För att kickstarta ett nytt C#-projekt, använd dotnet CLI (Command Line Interface). Här är grunderna:

```C#
// Öppna en terminal och kör följande:
dotnet new console -o MittProjekt

// Byt till projektets mapp:
cd MittProjekt

// Kör ditt nya projekt:
dotnet run

// Förväntad utmatning:
// Hello, World!
```

När du har koden ovan är du igång med en grundläggande "Hello, World!" applikation.

## Djupdykning:
Att starta ett projekt har sina rötter i de tidiga dagarna av programmering när allt var mer manuellt. Nu, med .NET Core och dess efterföljare, .NET 5/6/7, finns det flera sätt att börja:

- **dotnet CLI**: Det snabbaste sättet att skapa och köra C#-projekt.
- **Visual Studio**: Ett kraftfullt IDE med GUI-stöd för projektstrukturering.
- **Visual Studio Code**: Lätt och flexibelt, bra för både små och stora projekt.

Implementeringsdetaljer kan variera beroende på projekttypen - exempelvis webb, konsol eller mobilapplikation. Dotnet CLI tillåter förinställningar genom att använda olika projektmallar.

## Se även:
- [Microsofts officiella .NET-dokumentation](https://docs.microsoft.com/dotnet/core/tutorials/)
- [C# guide på Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [Gratis kurs på Codecademy](https://www.codecademy.com/learn/learn-c-sharp)

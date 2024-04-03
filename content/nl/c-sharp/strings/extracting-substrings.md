---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:45.271779-07:00
description: "Substrings extraheren is het actie van een specifiek deel van een tekenreeks\
  \ grijpen \u2014 een beetje zoals het uitscheppen van je favoriete stuk van een\u2026"
lastmod: '2024-03-13T22:44:50.799002-06:00'
model: gpt-4-0125-preview
summary: "Substrings extraheren is het actie van een specifiek deel van een tekenreeks\
  \ grijpen \u2014 een beetje zoals het uitscheppen van je favoriete stuk van een\
  \ taart."
title: Substrings extraheren
weight: 6
---

## Hoe:
C# maakt het trekken van substrings uit een tekenreeks eenvoudig. Hier is een snelle blik op hoe het gedaan wordt met de `Substring` methode en tekenreeks snijden met bereikoperators.

```C#
string volledigeTekenreeks = "Hallo, Wereld! Het leven is prachtig.";
// Met Substring(startIndex, lengte)
string geextraheerd1 = volledigeTekenreeks.Substring(7, 5); // "Wereld"

Console.WriteLine(geextraheerd1); // Uitvoer: Wereld

// Met tekenreeks snijden met bereikoperator [..]
string geextraheerd2 = volledigeTekenreeks[13..24]; // "Het leven is"

Console.WriteLine(geextraheerd2); // Uitvoer: Het leven is
```

## Diepgaand
Substrings zijn geen nieuwe truc. Ze bestaan al in talen zoals C en Java sinds jaar en dag. Echter, C# heeft het proces verfijnd met methodes en features die leesbaarheid en gebruiksgemak prioriteren.

Historisch gezien gebruikten programmeurs lussen en zorgvuldige indexberekeningen. De `Substring` methode in C# is een mooie upgrade. Het is eenvoudig - geef het een startindex en, optioneel, een lengte, en het doet het snijden voor je.

Het spektakel eindigt daar niet. Met C# 8.0 en verder zijn we geïntroduceerd aan bereikoperators zoals `[..]`. Ze maken natuurlijkere snijuitdrukkingen mogelijk, vooral bij het gebruik van indexen relatief aan het einde van de tekenreeks (aangeduid door de `^` operator).

Alternatieven voor `Substring` omvatten methodes zoals `Split`, Regex operaties, of tekenreeksmanipulatie met LINQ. De keuze hangt af van de situatie - je zou een CSV-lijn kunnen splitsen, een patroon met Regex kunnen zoeken, of substraten kunnen plukken met een fancy LINQ-uitdrukking.

Aan de implementatiekant zijn C# tekenreeksen onveranderlijk. Als je een substring neemt, verander je het origineel niet. In plaats daarvan maak je een nieuwe tekenreeks die een deel van de geheugenruimte van de ouder deelt - totdat je het wijzigt, en dan heeft het zijn eigen geheugentoewijzing.

## Zie ook
Als je dieper wilt duiken of gerelateerde onderwerpen wilt verkennen, hier zijn enkele bronnen:
- Officiële documentatie van Microsoft over `Substring`: https://docs.microsoft.com/nl-nl/dotnet/api/system.string.substring
- Meer over bereikoperators en indices in C#: https://docs.microsoft.com/nl-nl/dotnet/csharp/language-reference/proposals/csharp-8.0/ranges
- Tekenreeksmanipulatie met LINQ: https://docs.microsoft.com/nl-nl/dotnet/csharp/programming-guide/concepts/linq/
- Reguliere Expressies in C#: https://docs.microsoft.com/nl-nl/dotnet/standard/base-types/regular-expressions

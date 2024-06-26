---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:05.750794-07:00
description: 'Hoe te: Het concatenatie van strings in C# kan op verschillende manieren:
  Gebruikmakend van de `+` operator.'
lastmod: '2024-03-13T22:44:50.802037-06:00'
model: gpt-4-0125-preview
summary: Het concatenatie van strings in C# kan op verschillende manieren.
title: Samenvoegen van strings
weight: 3
---

## Hoe te:
Het concatenatie van strings in C# kan op verschillende manieren:

Gebruikmakend van de `+` operator:
```C#
string hallo = "Hallo";
string wereld = "Wereld";
string geconcateneerd = hallo + ", " + wereld + "!";
Console.WriteLine(geconcateneerd); // Uitvoer: Hallo, Wereld!
```

Gebruikmakend van de `String.Concat()` methode:
```C#
string geconcateneerd = String.Concat("Hallo", ", ", "Wereld", "!");
Console.WriteLine(geconcateneerd); // Uitvoer: Hallo, Wereld!
```

Gebruikmakend van `StringBuilder` voor efficiëntie in loops:
```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hallo");
sb.Append(", ");
sb.Append("Wereld");
sb.Append("!");
Console.WriteLine(sb.ToString()); // Uitvoer: Hallo, Wereld!
```

Gebruikmakend van stringinterpolatie (C# 6.0 en hoger):
```C#
string wereld = "Wereld";
string geconcateneerd = $"Hallo, {wereld}!";
Console.WriteLine(geconcateneerd); // Uitvoer: Hallo, Wereld!
```

## Diepgaand
Stringconcatenatie is niet nieuw; het bestaat al sinds de vroege dagen van programmering. Echter, de manier waarop we het in C# doen, is geëvolueerd. Oorspronkelijk werd de `+` veel gebruikt, maar het is niet altijd efficiënt, vooral binnen loops, omdat strings in .NET onveranderlijk zijn. Elke `+` operatie creëert een nieuwe string, wat kan leiden tot prestatieproblemen.

`String.Concat()` is een directe methode-aanroep die ook niet loop-vriendelijk is, maar prima voor een bekend, klein aantal strings.

`StringBuilder` is de gangbare methode voor scenario's met loops of wanneer een string stapsgewijs wordt opgebouwd. Onder de motorkap houdt `StringBuilder` een buffer bij om toevoegingen te accommoderen zonder voor elke append-operatie nieuwe strings te creëren.

Stringinterpolatie, geïntroduceerd in C# 6.0, zorgt voor meer leesbare en onderhoudsvriendelijke code. Het wordt omgezet in een `String.Format()`-aanroep op compileertijd, maar is makkelijker voor de ogen en minder gevoelig voor fouten.

Elke methode heeft zijn plaats: snelle concatenaties (`+`), het combineren van een paar strings (`String.Concat()`), zwaar stringwerk (`StringBuilder`), en schone, geformatteerde strings (stringinterpolatie).

## Zie Ook
- Microsoft Docs over Stringconcatenatie: [String Concatenation](https://docs.microsoft.com/nl-nl/dotnet/csharp/how-to/concatenate-multiple-strings)
- Microsoft Docs over `StringBuilder`: [StringBuilder Class](https://docs.microsoft.com/nl-nl/dotnet/api/system.text.stringbuilder)

---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:06:50.580373-07:00
description: "Stringinterpolatie in C# stelt je in staat om een nieuwe string te cre\xEB\
  ren door expressies binnen een stringliteral op te nemen, waardoor het\u2026"
lastmod: '2024-03-13T22:44:50.795991-06:00'
model: gpt-4-0125-preview
summary: "Stringinterpolatie in C# stelt je in staat om een nieuwe string te cre\xEB\
  ren door expressies binnen een stringliteral op te nemen, waardoor het\u2026"
title: Een string interpoleren
weight: 8
---

## Wat & Waarom?
Stringinterpolatie in C# stelt je in staat om een nieuwe string te creëren door expressies binnen een stringliteral op te nemen, waardoor het gemakkelijker wordt om strings te formatteren en te concatenaten. Programmeurs gebruiken deze functie om de leesbaarheid en het onderhoud van de code te verbeteren, met name wanneer ze te maken hebben met dynamische stringinhoud.

## Hoe te:
In C# wordt stringinterpolatie aangeduid met een dollarteken (`$`) gevolgd door een stringliteral. De variabelenamen of expressies worden tussen accolades (`{}`) geplaatst.

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Hallo, {name}! Je bent {age} jaar oud.";
Console.WriteLine(interpolatedString);
// Output: Hallo, Jane! Je bent 28 jaar oud.
```

In een complexer voorbeeld kun je binnen de accolades operaties uitvoeren of methoden aanroepen:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Totale prijs: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Output: Totale prijs: $59.97
```
De `:C2` formaatspecificeerder binnen de accolades formatteert het getal als een valuta met twee decimalen.

Voor scenario's die geavanceerdere formattering of lokalisatie vereisen, kun je overwegen de `string.Format` methode of bibliotheken zoals Humanizer te gebruiken. Humanizer kan strings, datums, tijden, tijdsintervallen, getallen en hoeveelheden manipuleren en weergeven in een meer leesbaar formaat voor mensen. Hieronder is een voorbeeld van het gebruik van Humanizer voor complexe stringmanipulatie. Merk op dat Humanizer geen deel uitmaakt van de standaard .NET-bibliotheek en vereist dat het NuGet-pakket `Humanizer` wordt geïnstalleerd.

Installeer eerst Humanizer via NuGet:

```
Install-Package Humanizer
```

Vervolgens kun je het als volgt gebruiken:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"Het evenement was {dayDifference} dagen geleden.".Humanize();
Console.WriteLine(humanized);
// Afhankelijk van de configuratie en cultuur, een mogelijke output: Het evenement was 5 dagen geleden.
```

Dit voorbeeld illustreert het basisgebruik. Humanizer ondersteunt een breed scala aan functionaliteiten die kunnen worden toegepast op strings, datums, getallen, en meer, waardoor je applicaties toegankelijker en intuïtiever worden.

---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:28.213938-07:00
description: "Een datum in de toekomst of het verleden berekenen houdt in dat je een\
  \ datum bepaalt die een gespecificeerd aantal dagen, maanden of jaren vanaf een\u2026"
lastmod: '2024-03-13T22:44:50.650492-06:00'
model: gpt-4-0125-preview
summary: Een datum in de toekomst of het verleden berekenen houdt in dat je een datum
  bepaalt die een gespecificeerd aantal dagen, maanden of jaren vanaf een gegeven
  datum verwijderd is.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe:
In Visual Basic for Applications (VBA) is de primaire functie die gebruikt wordt om toekomstige of verleden datums te berekenen `DateAdd()`. Deze functie voegt een gespecificeerd tijdsinterval toe aan een datum, en retourneert een nieuwe datum.

Hier is een basisvoorbeeld om 10 dagen aan de huidige datum toe te voegen:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' Voegt 10 dagen toe aan de huidige datum
Debug.Print futureDate ' Geeft iets weer zoals: 20-04-2023
```

Op een vergelijkbare manier om een datum 10 dagen in het verleden te vinden:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' Trekt 10 dagen af van de huidige datum
Debug.Print pastDate ' Geeft weer: 31-03-2023, uitgaande van vandaag is het 10-04-2023
```

Deze voorbeelden zijn vrij eenvoudig. Je kunt `"d"` vervangen door andere intervalcodes, zoals `"m"` voor maanden en `"yyyy"` voor jaren, om verschillende soorten datumcalculaties uit te voeren. Hier is hoe je een datum een jaar in de toekomst kunt berekenen:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' Voegt 1 jaar toe aan de huidige datum
Debug.Print nextYear ' Geeft weer: 10-04-2024 als vandaag 10-04-2023 is
```

## Diepere Verkenning
De functie `DateAdd` is een fundamenteel onderdeel van VBA sinds het begin, voortvloeiend uit zijn voorganger BASIC. Hoewel het gemak biedt voor het toevoegen of aftrekken van tijdsintervallen van datums, is het van vitaal belang om op te merken dat VBA, inclusief de functies voor datumhandling, niet altijd de gemak of efficiëntie kan bieden die in nieuwere programmeertalen te vinden is.

Zo bieden moderne talen zoals Python met de `datetime` module of JavaScript met bibliotheken zoals `moment.js` en `date-fns` meer intuïtieve en krachtige manieren voor datumanipulatie. Deze opties bieden betere ondersteuning voor lokalisatie, tijdzones en schrikkeljaren, wat hen geschikter kan maken voor toepassingen die nauwkeurige datumcalculaties op wereldwijde schaal vereisen.

Echter, voor Excel-macro's en toepassingen die integratie binnen het Microsoft Office-ecosysteem vereisen, blijft VBA een praktische keuze. De eenvoud van het direct toegang hebben tot en manipuleren van Excel-gegevens is een aanzienlijk voordeel. Bovendien biedt `DateAdd()` in VBA voor de meeste basisdatumcalculaties zoals planning en herinneringen, een adequate en eenvoudige oplossing. De syntax is gemakkelijk te begrijpen voor nieuwkomers, terwijl de integratie in de bredere Office-suite toepassingen de relevantie ervan garandeert in specifieke gebruiksgevallen.

Concluderend, hoewel alternatieve programmeertalen modernere benaderingen van datumcalculatie kunnen bieden, dient `DateAdd()` in VBA als een getuigenis van de blijvende kracht van de taal in de domeinen waar het het meest nodig is.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:27.046954-07:00
description: "Teksten zoeken en vervangen in Dart omvat het onderzoeken van strings\
  \ om bepaalde patronen of opeenvolgingen van karakters te vinden en deze te vervangen\u2026"
lastmod: '2024-03-13T22:44:50.489101-06:00'
model: gpt-4-0125-preview
summary: "Teksten zoeken en vervangen in Dart omvat het onderzoeken van strings om\
  \ bepaalde patronen of opeenvolgingen van karakters te vinden en deze te vervangen\u2026"
title: Tekst zoeken en vervangen
---

{{< edit_this_page >}}

## Wat & Waarom?

Teksten zoeken en vervangen in Dart omvat het onderzoeken van strings om bepaalde patronen of opeenvolgingen van karakters te vinden en deze te vervangen door nieuwe inhoud. Deze operatie is fundamenteel voor taken zoals gegevensvalidatie, het formatteren van uitvoer, het analyseren van gebruikersinvoer of zelfs het manipuleren van URL's en bestandspaden, waardoor applicaties dynamischer worden en beter reageren op gebruikersbehoeften.

## Hoe te:

Dart biedt robuuste methoden voor het zoeken en vervangen van tekst rechtstreeks via zijn `String` klasse, zonder de noodzaak voor externe bibliotheken. Hier is hoe je het kunt doen:

### Basis Zoeken en Vervangen

Om naar een substring te zoeken en deze te vervangen door een andere string, kun je `replaceAll` gebruiken:

```dart
String sampleText = "Hallo, Dart! Dart is geweldig.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Uitvoer: Hallo, Flutter! Flutter is geweldig.
```

### Gebruikmakend van Reguliere Expressies

Voor complexere zoek- en vervangbehoeften gebruikt Dart reguliere expressies via de `RegExp` klasse. Dit maakt patroonherkenning en vervanging in strings mogelijk:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Uitvoer: Dart 2024, Flutter 2024
```

Dit voorbeeld vindt alle instanties van één of meer cijfers (`\d+`) in de string en vervangt deze door "2024".

### Zoeken Zonder rekening te Houden met Hoofdletters/Kleine Letters

Om een zoekactie uit te voeren zonder rekening te houden met hoofdletters/kleine letters, kun je de `RegExp` constructor wijzigen om hoofdlettergevoeligheid te negeren:

```dart
String sampleText = "Welkom bij Dart, de programmeertaal.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Uitvoer: Welkom bij Flutter, de programmeertaal.
```

### Vervangen met een Functie

Voor dynamische vervangingen op basis van de match zelf, staat Dart het doorgeven van een functie aan `replaceAllMapped` toe. Deze functie kan operaties of berekeningen uitvoeren op de overeenkomende sequenties:

```dart
String sampleText = "Verhoog 5 met 1 om 6 te krijgen.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Uitvoer: Verhoog 6 met 1 om 7 te krijgen.
```

Dit vervangt elke cijferreeks door zijn verhoogde waarde. Elke match wordt omgezet in een integer, verhoogd en vervolgens terug omgezet naar een string voor vervanging.

Darts mogelijkheden voor stringmanipulatie, in het bijzonder voor het zoeken en vervangen van tekst, maken het een krachtige tool voor het verwerken en voorbereiden van gegevens binnen je applicaties. Of je nu eenvoudige stringvervangingen gebruikt of de kracht van reguliere expressies benut, Dart biedt de flexibiliteit en prestaties die nodig zijn voor effectieve tekstmanipulatie.

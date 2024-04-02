---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:32.944193-07:00
description: "Het lezen van een tekstbestand in Dart omvat het toegang krijgen tot\
  \ en het ophalen van gegevens uit bestanden die zijn opgeslagen op het bestandssysteem.\u2026"
lastmod: '2024-03-13T22:44:50.523974-06:00'
model: gpt-4-0125-preview
summary: "Het lezen van een tekstbestand in Dart omvat het toegang krijgen tot en\
  \ het ophalen van gegevens uit bestanden die zijn opgeslagen op het bestandssysteem.\u2026"
title: Een tekstbestand lezen
weight: 22
---

## Wat & Waarom?

Het lezen van een tekstbestand in Dart omvat het toegang krijgen tot en het ophalen van gegevens uit bestanden die zijn opgeslagen op het bestandssysteem. Programmeurs doen dit om invoergegevens, configuratie-instellingen of datasets te verwerken, wat het een fundamentele bewerking maakt voor veel applicaties, variërend van eenvoudige scripts tot complexe apps.

## Hoe te:

Dart's kernbibliotheek, `dart:io`, biedt de nodige functionaliteiten om tekstbestanden synchroon of asynchroon te lezen. Hier is hoe je beide benadert.

**Synchroon:**

```dart
import 'dart:io';

void main() {
  var bestandsnaam = "pad/naar/jouw/tekstbestand.txt";
  var bestand = File(bestandsnaam);

  // Het bestand synchroon lezen
  var inhoud;
  try {
    inhoud = bestand.readAsStringSync();
    print(inhoud);
  } catch (e) {
    print('Fout bij het lezen van bestand: $e');
  }
}
```

**Asynchroon:**

Om te voorkomen dat het programma wordt geblokkeerd terwijl het bestand wordt gelezen, vooral nuttig voor grote bestanden of responsieve applicaties:

```dart
import 'dart:io';

void main() async {
  var bestandsnaam = "pad/naar/jouw/tekstbestand.txt";
  var bestand = File(bestandsnaam);

  try {
    String inhoud = await bestand.readAsString();
    print(inhoud);
  } catch (e) {
    print('Fout bij het lezen van bestand: $e');
  }
}
```

**Voorbeelduitvoer:**

Als je tekstbestand bevat:

```
Hallo, Dart!
```

Zullen beide van de bovenstaande methoden uitvoeren:

```
Hallo, Dart!
```

**Gebruikmakend van een Bibliotheek van Derden:**

Voor extra functies zoals vereenvoudigde bestandsbewerkingen of verbeterde foutafhandeling, zou je kunnen overwegen om bibliotheken van derden te gebruiken zoals `package:file`. Echter, op basis van mijn laatste update, is het direct gebruik van het kern `dart:io` pakket, zoals hierboven getoond, de meest gebruikelijke en eenvoudige methode voor het lezen van tekstbestanden in Dart.

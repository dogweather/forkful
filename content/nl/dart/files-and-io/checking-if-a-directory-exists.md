---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:55.379387-07:00
description: "Controleren of een map bestaat in Dart gaat over het verifi\xEBren van\
  \ de aanwezigheid van een map op een gespecificeerd pad op het bestandssysteem voordat\u2026"
lastmod: '2024-03-13T22:44:50.520872-06:00'
model: gpt-4-0125-preview
summary: "Controleren of een map bestaat in Dart gaat over het verifi\xEBren van de\
  \ aanwezigheid van een map op een gespecificeerd pad op het bestandssysteem voordat\u2026"
title: Controleren of een map bestaat
weight: 20
---

## Wat & Waarom?

Controleren of een map bestaat in Dart gaat over het verifiëren van de aanwezigheid van een map op een gespecificeerd pad op het bestandssysteem voordat operaties zoals het lezen of schrijven van bestanden worden uitgevoerd. Programmeurs doen dit om fouten te voorkomen die optreden wanneer geprobeerd wordt om mappen die niet bestaan te benaderen of te wijzigen.

## Hoe doe je dat:

Dart gebruikt de `dart:io` bibliotheek om met bestanden en mappen te werken. Hier is een simpele manier om te controleren of een map bestaat:

```dart
import 'dart:io';

void main() {
  var directory = Directory('pad/naar/jouw/map');

  if (directory.existsSync()) {
    print('Map bestaat');
  } else {
    print('Map bestaat niet');
  }
}
```
Voorbeelduitvoer als de map bestaat:
```
Map bestaat
```

Of, als deze niet bestaat:
```
Map bestaat niet
```

Om meer complexe scenario's aan te pakken, zoals asynchroon controleren of een map creëren als deze niet bestaat, kun je de volgende aanpak gebruiken:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('pad/naar/jouw/map');

  // Asynchroon controleren of de map bestaat
  var exists = await directory.exists();
  if (exists) {
    print('Map bestaat');
  } else {
    print('Map bestaat niet, wordt gecreëerd...');
    await directory.create(); // Dit creëert de map
    print('Map gecreëerd');
  }
}
```

Voorbeelduitvoer als de map niet bestond en is gecreëerd:
```
Map bestaat niet, wordt gecreëerd...
Map gecreëerd
```

De ingebouwde mogelijkheden van Dart zijn gewoonlijk voldoende voor het omgaan met bestanden en mappen, dus externe bibliotheken zijn meestal niet nodig voor deze taak. Echter, voor meer complexe bestandssysteemoperaties, kunnen pakketten zoals `path` (voor het manipuleren van paden op een platformagnostische manier) de `dart:io` bibliotheek aanvullen, maar bieden niet direct meer geavanceerde controles op de aanwezigheid van mappen dan wat hier is getoond.

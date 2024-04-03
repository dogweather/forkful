---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:37.767881-07:00
description: "\xC5 sjekke om en mappe eksisterer i Dart handler om \xE5 verifisere\
  \ tilstedev\xE6relsen av en mappe p\xE5 en angitt bane i filsystemet f\xF8r man\
  \ utf\xF8rer operasjoner\u2026"
lastmod: '2024-03-13T22:44:40.503508-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sjekke om en mappe eksisterer i Dart handler om \xE5 verifisere tilstedev\xE6\
  relsen av en mappe p\xE5 en angitt bane i filsystemet f\xF8r man utf\xF8rer operasjoner\
  \ som \xE5 lese eller skrive filer."
title: Sjekker om en katalog eksisterer
weight: 20
---

## Hva & Hvorfor?

Å sjekke om en mappe eksisterer i Dart handler om å verifisere tilstedeværelsen av en mappe på en angitt bane i filsystemet før man utfører operasjoner som å lese eller skrive filer. Programmerere gjør dette for å unngå feil som oppstår når man prøver å få tilgang til eller modifisere mapper som ikke eksisterer.

## Hvordan:

Dart bruker `dart:io`-biblioteket for å arbeide med filer og mapper. Her er en enkel måte å sjekke om en mappe eksisterer:

```dart
import 'dart:io';

void main() {
  var directory = Directory('sti/til/din/mappe');

  if (directory.existsSync()) {
    print('Mappen eksisterer');
  } else {
    print('Mappen eksisterer ikke');
  }
}
```
Eksempel på utskrift hvis mappen eksisterer:
```
Mappen eksisterer
```

Eller, hvis den ikke gjør det:
```
Mappen eksisterer ikke
```

For å håndtere mer komplekse scenarioer, som asynkron sjekking eller å opprette en mappe hvis den ikke eksisterer, kan du bruke følgende tilnærming:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('sti/til/din/mappe');

  // Sjekker asynkront om mappen eksisterer
  var exists = await directory.exists();
  if (exists) {
    print('Mappen eksisterer');
  } else {
    print('Mappen eksisterer ikke, oppretter...');
    await directory.create(); // Dette oppretter mappen
    print('Mappe opprettet');
  }
}
```

Eksempel på utskrift hvis mappen ikke eksisterte og ble opprettet:
```
Mappen eksisterer ikke, oppretter...
Mappe opprettet
```

Darts innebygde muligheter er vanligvis tilstrekkelige for å håndtere filer og mapper, så tredjeparts biblioteker er vanligvis ikke nødvendige for denne oppgaven. Imidlertid, for mer komplekse filsystemoperasjoner, kan pakker som `path` (for å manipulere baner på en plattformagnostisk måte) komplementere `dart:io`-biblioteket, men tilbyr ikke direkte mer avanserte sjekker av mappeeksistens enn det som er vist.

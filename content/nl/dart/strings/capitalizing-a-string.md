---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:14.007126-07:00
description: "Het kapitaliseren van een string houdt in dat je de eerste letter van\
  \ een woord of een hele zin wijzigt naar een hoofdletter, terwijl je de rest van\
  \ de\u2026"
lastmod: '2024-03-09T21:06:14.673596-07:00'
model: gpt-4-0125-preview
summary: "Het kapitaliseren van een string houdt in dat je de eerste letter van een\
  \ woord of een hele zin wijzigt naar een hoofdletter, terwijl je de rest van de\u2026"
title: Een string met hoofdletters
---

{{< edit_this_page >}}

## Wat & Waarom?

Het kapitaliseren van een string houdt in dat je de eerste letter van een woord of een hele zin wijzigt naar een hoofdletter, terwijl je de rest van de karakters laat zoals ze zijn. Programmeurs gebruiken deze techniek vaak bij het formatteren van gebruikersinvoer of het weergeven van tekst om consistentie te waarborgen of om grammaticale regels in gebruikersinterfaces te volgen.

## Hoe te:

### Gebruikmakend van Dart's Ingebouwde Methoden

Dart biedt eenvoudige, directe methoden voor stringmanipulatie. Om een woord of een zin te kapitaliseren, neem je doorgaans het eerste karakter, converteer je dit naar een hoofdletter en voeg je het vervolgens samen met de rest van de string. Hier is hoe je het kunt implementeren:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var voorbeeld = "hallo wereld";
  print(capitalize(voorbeeld)); // Uitvoer: Hallo wereld
}
```

### Elk Woord Kapitaliseren

Om de eerste letter van elk woord in een string te kapitaliseren, kun je de string opsplitsen in woorden, elk woord kapitaliseren en ze vervolgens weer samenvoegen:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var voorbeeld = "hallo dart enthousiastelingen";
  print(capitalizeWords(voorbeeld)); // Uitvoer: Hallo Dart Enthousiastelingen
}
```

### Gebruikmaken van Derde-partij Bibliotheken

Hoewel Dart's standaardbibliotheek de basisbehoeften dekt, kunnen bepaalde taken handiger worden uitgevoerd met behulp van derde-partij pakketten. Een populaire keuze voor uitgebreide stringmanipulatie mogelijkheden, inclusief kapitalisatie, is het [`recase`](https://pub.dev/packages/recase) pakket. Nadat je het aan de `pubspec.yaml` van je project hebt toegevoegd, kun je eenvoudig strings kapitaliseren onder andere functionaliteiten:

```dart
import 'package:recase/recase.dart';

void main() {
  var voorbeeld = "hallo wereld";
  var rc = ReCase(voorbeeld);

  print(rc.titleCase); // Uitvoer: Hallo Wereld
}
```

Met `recase` kun je individuele woorden, hele zinnen kapitaliseren of zelfs andere casing conventies volgen zonder handmatig de string transformaties te hoeven afhandelen.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:05.085934-07:00
description: "Een string omzetten naar kleine letters is een fundamentele bewerking\
  \ waarbij alle tekens in een gegeven string worden getransformeerd naar hun\u2026"
lastmod: '2024-03-09T21:06:14.677282-07:00'
model: gpt-4-0125-preview
summary: "Een string omzetten naar kleine letters is een fundamentele bewerking waarbij\
  \ alle tekens in een gegeven string worden getransformeerd naar hun\u2026"
title: Een string converteren naar onderkast
---

{{< edit_this_page >}}

## Wat & Waarom?

Een string omzetten naar kleine letters is een fundamentele bewerking waarbij alle tekens in een gegeven string worden getransformeerd naar hun equivalenten in kleine letters. Programmeurs voeren deze bewerking doorgaans uit om hoofdletterongevoelige vergelijkingen te bereiken of om tekstinput te standaardiseren voor verdere verwerking, waardoor applicaties gebruiksvriendelijker worden en data consistenter.

## Hoe:

In Dart kun je een string omzetten naar kleine letters met behulp van de `toLowerCase()` methode die door de `String` klasse wordt aangeboden. Deze methode retourneert een nieuwe string waarin alle hoofdletters zijn omgezet naar kleine letters. Laten we eens kijken hoe dit werkt met een eenvoudig voorbeeld:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Output: hello, world!
}
```

Dart vereist geen externe bibliotheken voor basis stringmanipulatietaken, inclusief het omzetten naar kleine letters, aangezien de `String` klasse van de standaardbibliotheek vrij uitgebreid is. Echter, voor complexere manipulaties die locatiespecifieke regels omvatten, zou je kunnen overwegen om het `intl` pakket te gebruiken, dat internationalisatie- en lokalisatiefaciliteiten biedt, inclusief omzetting van hoofdletters gebaseerd op locatie:

Om `intl` te gebruiken, voeg het toe aan je `pubspec.yaml` bestand:

```yaml
dependencies:
  intl: ^0.17.0
```

Vervolgens kun je de methode `toLocaleLowerCase()` gebruiken om een string om te zetten naar kleine letters op basis van specifieke locaties:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Turkse Locatie
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Output: istanbul
  
  // Standaard Locatie (en)
  print(originalString.toLowerCase()); // Output: i̇stanbul
}
```

In dit voorbeeld, let op hoe de Turkse locatie correct omgaat met de puntloze 'i', wat het belang van locatiebewuste transformaties in geïnternationaliseerde applicaties benadrukt.

---
title:                "Schrijven naar standaardfout"
date:                  2024-03-08T21:58:02.675631-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) in Dart gaat over het verzenden van foutmeldingen en diagnostische gegevens naar een aparte stroom, los van de standaarduitvoer (stdout). Programmeurs doen dit om onderscheid te maken tussen normale programma-uitvoer en fouten of waarschuwingsberichten, waardoor het gemakkelijker wordt om te debuggen en loggen.

## Hoe:

In Dart is schrijven naar stderr eenvoudig met behulp van het `stderr`-object dat beschikbaar is in `dart:io`. Hier is een basisvoorbeeld:

```dart
import 'dart:io';

void main() {
  stderr.writeln('Dit is een foutmelding.');
}
```

Uitvoer bij uitvoering:
```
Dit is een foutmelding.
```
Dit bericht wordt naar de stderr-stroom gestuurd, die typisch wordt weergegeven in de console of terminal.

Om meer complexiteit te demonstreren, zoals het loggen van een uitzondering, staat de rijke set functies van Dart toe voor bondige en effectieve foutafhandeling:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Simuleer een bewerking die een uitzondering kan veroorzaken
    throw Exception('Er is iets fout gegaan!');
  } catch (e) {
    stderr.writeln('Fout: $e');
  }
}

void main() {
  riskyOperation();
}
```

Uitvoer bij uitvoering:
```
Fout: Exception: Er is iets fout gegaan!
```

Dit patroon is vooral nuttig voor applicaties die normale logs van foutlogs moeten scheiden, waardoor het gemakkelijker wordt om applicaties te monitoren en debuggen.

Hoewel Dart's standaardbibliotheek vrij uitgebreid is, hebben veel programma's geen externe bibliotheken nodig voor het schrijven naar stderr. Echter, als je applicatie meer geavanceerde loggingmogelijkheden nodig heeft (bijv. naar bestanden, over het netwerk, formatteren), is het `logging`-pakket een populaire keuze. Hier is een snelle blik op het gebruik van `logging` voor fouten:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Ernstige fout: Er is iets aanzienlijk slechts gebeurd.');
}
```

Uitvoer bij uitvoering:
```
SEVERE: 2023-04-01 00:00:00.000: Ernstige fout: Er is iets aanzienlijk slechts gebeurd.
```

Deze methode biedt een hogere mate van aanpassing en controle over wat als een fout wordt gelogd en hoe het wordt geformatteerd, wat erg handig kan zijn in grotere, complexere applicaties.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:25.668928-07:00
description: "Debug-uitvoer afdrukken in Dart gaat over het weergeven van informatie\
  \ in de console tijdens runtime, waardoor ontwikkelaars de uitvoeringsstroom kunnen\u2026"
lastmod: '2024-03-09T21:06:14.691297-07:00'
model: gpt-4-0125-preview
summary: "Debug-uitvoer afdrukken in Dart gaat over het weergeven van informatie in\
  \ de console tijdens runtime, waardoor ontwikkelaars de uitvoeringsstroom kunnen\u2026"
title: Debug-uitvoer afdrukken
---

{{< edit_this_page >}}

## Wat & Waarom?

Debug-uitvoer afdrukken in Dart gaat over het weergeven van informatie in de console tijdens runtime, waardoor ontwikkelaars de uitvoeringsstroom kunnen volgen, de staat van variabelen kunnen onderzoeken, of de bron van fouten kunnen identificeren. Programmeurs gebruiken het vaak voor probleemoplossing en om te verifiëren dat hun code zich gedraagt zoals verwacht, wat een soepeler en efficiënter ontwikkelingsproces bevordert.

## Hoe doe je dat:

In Dart kun je debug-uitvoer afdrukken met de `print()` functie. Hier is hoe je eenvoudige berichten en variabele waarden uitvoert:

```dart
void main() {
  String groet = "Hallo, Dart!";
  print(groet); // Print: Hallo, Dart!

  int nummer = 42;
  print('Het nummer is $nummer.'); // Print: Het nummer is 42.
}
```

Voor gestructureerde gegevens, zoals lijsten of objecten, biedt Dart's `toString()` methode mogelijk niet genoeg detail. In die gevallen kun je de `jsonEncode` functie gebruiken uit Dart’s `dart:convert` bibliotheek om de gegevens naar een JSON-string te converteren voor een beter leesbare uitvoer:

```dart
import 'dart:convert';

void main() {
  var gebruiker = {
    'naam': 'John Doe',
    'leeftijd': 30,
    'emails': ['john.doe@voorbeeld.com', 'john@voorbeeld.com'],
  };

  print(jsonEncode(gebruiker));
  // Print: {"naam":"John Doe","leeftijd":30,"emails":["john.doe@voorbeeld.com","john@voorbeeld.com"]}
}
```

Wanneer geavanceerdere debugmogelijkheden nodig zijn, zoals loggen met verschillende niveaus van belangrijkheid (info, waarschuwing, fout), kun je gebruikmaken van externe bibliotheken zoals `logger’. Zo gebruik je het:

1. Voeg `logger` toe aan je `pubspec.yaml`:

```yaml
afhankelijkheden:
  logger: ^1.0.0
```

2. Gebruik `logger` in je Dart-code:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Dit is een debug bericht");
  logger.w("Dit is een waarschuwingsbericht");
  logger.e("Dit is een foutbericht");
}
```

De uitvoer zal informatiever zijn, waarbij het niveau van het bericht en het bericht zelf worden getoond, waardoor het gemakkelijker is om te onderscheiden tussen verschillende soorten logberichten.

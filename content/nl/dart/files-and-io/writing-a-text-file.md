---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:47.686788-07:00
description: "Het schrijven van een tekstbestand in Dart houdt in dat bestanden op\
  \ de schijf worden gemaakt of gewijzigd om gegevens op te slaan in een leesbaar\u2026"
lastmod: '2024-03-13T22:44:50.525037-06:00'
model: gpt-4-0125-preview
summary: "Het schrijven van een tekstbestand in Dart houdt in dat bestanden op de\
  \ schijf worden gemaakt of gewijzigd om gegevens op te slaan in een leesbaar\u2026"
title: Een tekstbestand schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?
Het schrijven van een tekstbestand in Dart houdt in dat bestanden op de schijf worden gemaakt of gewijzigd om gegevens op te slaan in een leesbaar formaat. Programmeurs doen dit om applicatiegegevens, configuraties, logs of informatie die tussen applicatieruns moet blijven bestaan of gegevens moet delen met andere applicaties of gebruikers, op te slaan.

## Hoe:
De kernbibliotheek van Dart biedt het `dart:io` pakket voor bestandsbehandeling, waarmee je tekstbestanden kunt schrijven zonder de noodzaak voor bibliotheken van derden. Hier is een eenvoudig voorbeeld van het schrijven van een tekstbestand:

```dart
import 'dart:io';

void main() async {
  // Maak een nieuw bestand genaamd 'example.txt' in de huidige map.
  var file = File('example.txt');
  
  // Schrijf een string naar het bestand.
  await file.writeAsString('Hallo, Dart!');
  
  // Verifieer de inhoud.
  print(await file.readAsString()); // Output: Hallo, Dart!
}
```

Wanneer je te maken hebt met grotere bestanden of datastromen, geef je wellicht de voorkeur aan het schrijven van inhoud met `openWrite`, dat een `IOSink` teruggeeft en je in staat stelt om gegevens in delen te schrijven:

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // Schrijf meerdere regels naar het bestand.
  sink
    ..writeln('Regel 1: De snelle bruine vos springt over de luie hond.')
    ..writeln('Regel 2: Dart is geweldig!')
    ..close();

  // Wacht tot de sink gesloten is om te verzekeren dat alle gegevens naar het bestand zijn geschreven.
  await sink.done;

  // Lees en print de inhoud van het bestand om te verifiëren
  print(await file.readAsString());
}
```

Voor meer geavanceerde bestandsbewerkingen, waaronder toevoegen aan bestanden of bytes schrijven, kun je dieper ingaan op de methoden van de `File` klasse aangeboden door `dart:io`. Daarnaast kan het voor grote schaal of meer complexe projecten nuttig zijn om pakketten zoals `path` voor de omgang met bestandspaden of `shelf` voor functionaliteiten van webserver te overwegen, hoewel directe bestandsschrijving meestal afhankelijk is van de ingebouwde Dart-bibliotheken.

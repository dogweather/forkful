---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:28.318611-07:00
description: "\xC5 beregne en dato i fremtiden eller fortiden er en vanlig oppgave\
  \ for programmerere, som handler om planlegging, p\xE5minnelser, eller enhver funksjon\
  \ som\u2026"
lastmod: '2024-03-13T22:44:40.502192-06:00'
model: gpt-4-0125-preview
summary: "\xC5 beregne en dato i fremtiden eller fortiden er en vanlig oppgave for\
  \ programmerere, som handler om planlegging, p\xE5minnelser, eller enhver funksjon\
  \ som avhenger av datoberegninger."
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Hvordan:
Dart gir solid støtte for datomanipulering gjennom sin `DateTime`-klasse. Her er hvordan du kan beregne fremtidige eller tidligere datoer ved å bruke nativ Dart, uten å trenge tredjepartsbiblioteker.

### Beregning av en fremtidig dato
For å beregne en dato i fremtiden, lager du et `DateTime`-objekt og bruker `add`-metoden med ønsket varighet.

```dart
DateTime iDag = DateTime.now();
Duration tiDager = Duration(days: 10);
DateTime fremtidigDato = iDag.add(tiDager);

print(fremtidigDato); // Utdata: 2023-04-21 14:22:35.123456 (eksempel på utdata, avhenger av gjeldende dato og tid)
```

### Beregning av en tidligere dato
For å beregne en dato i fortiden, bruker du `subtract`-metoden på et `DateTime`-objekt med nødvendig varighet.

```dart
DateTime iDag = DateTime.now();
Duration femtenDagerSiden = Duration(days: 15);
DateTime tidligereDato = iDag.subtract(femtenDagerSiden);

print(tidligereDato); // Utdata: 2023-03-27 14:22:35.123456 (eksempel på utdata, avhenger av gjeldende dato og tid)
```

### Bruk av tredjepartsbiblioteker
Selv om Darts native kapabiliteter for datomanipulering er kraftfulle, kan du finne deg selv i behov av mer spesifikke operasjoner, slik som å parse eller formatere datoer enklere, eller utføre komplekse beregninger. I slike tilfeller kan `time`-pakken være svært nyttig.

Først, legg `time` til dine `pubspec.yaml`-avhengigheter:

```yaml
dependencies:
  time: ^2.0.0
```

Deretter kan du bruke den til å utføre lignende beregninger med forbedret lesbarhet:

```dart
import 'package:time/time.dart';

void main() {
  DateTime iDag = DateTime.now();

  // Beregning av en fremtidig dato
  DateTime fremtidigDato = iDag + 10.dager;
  print(fremtidigDato); // Utdataformat: 2023-04-21 14:22:35.123456

  // Beregning av en tidligere dato
  DateTime tidligereDato = iDag - 15.dager;
  print(tidligereDato); // Utdataformat: 2023-03-27 14:22:35.123456
}
```

Disse eksemplene illustrerer grunnleggende datomanipulasjoner i Dart, inkludert å legge til og trekke fra tid til eller fra en nåværende dato, og demonstrerer hvor enkelt datoer kan håndteres i Dart-applikasjoner.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:45.998674-07:00
description: "Hvordan: I Dart kan du sammenligne datoer ved \xE5 bruke `DateTime`-klassen,\
  \ som tilbyr metoder som `isBefore`, `isAfter`, og `isAtSameMomentAs` for direkte\u2026"
lastmod: '2024-03-13T22:44:40.501109-06:00'
model: gpt-4-0125-preview
summary: "I Dart kan du sammenligne datoer ved \xE5 bruke `DateTime`-klassen, som\
  \ tilbyr metoder som `isBefore`, `isAfter`, og `isAtSameMomentAs` for direkte sammenligning."
title: Sammenligner to datoer
weight: 27
---

## Hvordan:
I Dart kan du sammenligne datoer ved å bruke `DateTime`-klassen, som tilbyr metoder som `isBefore`, `isAfter`, og `isAtSameMomentAs` for direkte sammenligning. I tillegg kan forskjellen mellom datoer bestemmes ved å bruke `difference()`-metoden, som gir et `Duration`-objekt som detaljerer tidsrommet mellom de to tidspunktene.

Her er et grunnleggende eksempel som illustrerer disse konseptene:

```dart
void main() {
  DateTime eventStart = DateTime(2023, 5, 15);
  DateTime eventEnd = DateTime(2023, 5, 20);
  
  // Sjekker om en dato er før en annen
  if (eventStart.isBefore(eventEnd)) {
    print("Arrangementets startdato er før sluttdatoen.");
  }

  // Sjekker om to datoer er de samme
  if (!eventStart.isAtSameMomentAs(eventEnd)) {
    print("Startdatoen og sluttdatoen er ikke den samme.");
  }
  
  // Beregner forskjellen mellom to datoer
  Duration eventDuration = eventEnd.difference(eventStart);
  print("Arrangementet varer i ${eventDuration.inDays} dager.");
}

/*
Output:
Arrangementets startdato er før sluttdatoen.
Startdatoen og sluttdatoen er ikke den samme.
Arrangementet varer i 5 dager.
*/
```

For mer avanserte dato-manipulasjoner, som formatkonverteringer, kan du finne `DateFormat`-klassen fra `intl`-pakken nyttig. Nedenfor er et eksempel som demonstrerer hvordan du bruker den for formatering og sammenligning av datoer:

Først, inkluder `intl`-pakken i din `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Deretter, bruk den som følger:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime departureDate = DateTime(2023, 5, 15);
  DateTime returnDate = DateTime.parse('2023-05-20');

  // Formaterer datoer
  var formatter = DateFormat('yyyy-MM-dd');
  print("Avgang: ${formatter.format(departureDate)}");
  print("Retur: ${formatter.format(returnDate)}");

  // Sammenligner ved hjelp av formaterte strenger
  if (formatter.format(departureDate) == formatter.format(returnDate)) {
    print("Avgangs- og returdatoene er de samme.");
  } else {
    print("Avgangs- og returdatoene er forskjellige.");
  }
}

/*
Output:
Avgang: 2023-05-15
Retur: 2023-05-20
Avgangs- og returdatoene er forskjellige.
*/
```

Dette eksempelet viser hvordan man kan sammenligne to `DateTime`-objekter både direkte og ved å bruke formaterte strenger for sammenligninger som trenger å ignorere spesifikke komponenter som tid.

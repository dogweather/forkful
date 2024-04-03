---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:57.273414-07:00
description: "Hvordan: Dart tilbyr en grei syntaks for \xE5 opprette og manipulere\
  \ Maps. Nedenfor er eksempler som demonstrerer grunnleggende operasjoner som oppretting,\
  \ \xE5\u2026"
lastmod: '2024-03-13T22:44:40.479529-06:00'
model: gpt-4-0125-preview
summary: "Dart tilbyr en grei syntaks for \xE5 opprette og manipulere Maps."
title: Bruke assosiativ tabeller
weight: 15
---

## Hvordan:
Dart tilbyr en grei syntaks for å opprette og manipulere Maps. Nedenfor er eksempler som demonstrerer grunnleggende operasjoner som oppretting, å legge til elementer og hente verdier.

```dart
void main() {
  // Opprette en map
  var fruktFarger = {
    'eple': 'rød',
    'banan': 'gul',
    'drue': 'lilla'
  };

  // Legge til et nytt nøkkel-verdi-par
  fruktFarger['appelsin'] = 'oransje';

  // Få tilgang til en verdi med nøkkelen
  print(fruktFarger['eple']); // Utdata: rød

  // Oppdatere en verdi
  fruktFarger['banan'] = 'grønn';

  // Iterere over Mappen
  fruktFarger.forEach((frukt, farge) {
    print('$frukt: $farge');
  });
  // Eksempel på utdata:
  // eple: rød
  // banan: grønn
  // drue: lilla
  // appelsin: oransje
}
```

For komplekse datastrukturer eller utvidet funksjonalitet, stoler Dart-programmerere ofte på tillegg til biblioteker. Et slikt bibliotek er `collection` som tilbyr avanserte samlingstyper og verktøy. Selv om `collection` ikke endrer den grunnleggende måten Maps håndteres på, beriker det dem med nyttefunksjoner og mer sofistikerte samlingstyper. Her er hvordan du kan bruke det for en mer spesifikk oppgave, som å sortere en Map etter dens verdier:

Først, sørg for at `collection` pakken er inkludert i din `pubspec.yaml`-fil:

```yaml
dependencies:
  collection: ^1.15.0
```

Deretter kan du bruke den slik:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruktFarger = {
    'eple': 'rød',
    'banan': 'gul',
    'drue': 'lilla',
    'appelsin': 'oransje'
  };

  // Sortere Mappen etter dens verdier (farger)
  var sorterteFrukterEtterFarge = SplayTreeMap.from(
    fruktFarger,
    (nøkkel1, nøkkel2) => fruktFarger[nøkkel1]!.compareTo(fruktFarger[nøkkel2]!)
  );

  print(sorterteFrukterEtterFarge);
  // Utdata:
  // {appelsin: oransje, eple: rød, banan: gul, drue: lilla}
}
```

Dette eksemplet demonstrerer sortering av en Maps oppføringer basert på deres verdier, og viser hvordan Dart og dets livlige økosystem kan håndtere associative tabeller for mer sofistikert datahåndtering.

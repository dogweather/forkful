---
title:                "Få den nåværende datoen"
date:                  2024-03-08T21:54:42.601248-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få tak i den nåværende datoen i Dart innebærer å spørre systemet om den nåværende datoen og tiden. Denne funksjonaliteten brukes ofte i applikasjoner for funksjoner som tidsstempling av hendelser, vise den nåværende datoen til brukere, eller beregning av varigheter. Å vite hvordan man effektivt henter og manipulerer den nåværende datoen er grunnleggende for planlegging, logging, og tidsfølsomme funksjoner.

## Hvordan:
Darts kjerneprogrambibliotek gir enkel tilgang til den nåværende datoen og tiden gjennom `DateTime`-klassen. Her er et grunnleggende eksempel for å få den nåværende datoen:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Eksempelutskrift: 2023-04-12 10:00:00.000
}
```

Hvis du bare trenger datodelen (år, måned, dag), kan du formatere `DateTime`-objektet:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Eksempelutskrift: 2023-04-12
}
```

Dart inkluderer ikke et innebygd bibliotek for mer kompleks datoformatering, men du kan bruke `intl`-pakken for dette formålet. Først legger du til pakken i din `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Deretter kan du enkelt formatere datoer:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Eksempelutskrift: 2023-04-12
}
```

For mer avanserte formateringsalternativer, utforsk `DateFormat`-klassen som tilbys av `intl`-pakken, som støtter et bredt spekter av mønstre og lokaliteter.

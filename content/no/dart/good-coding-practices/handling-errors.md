---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:21.530737-07:00
description: "H\xE5ndtering av feil i Dart handler om \xE5 forutse og h\xE5ndtere\
  \ unntak som oppst\xE5r under programkj\xF8ring for \xE5 forbedre p\xE5liteligheten\
  \ og brukervennligheten.\u2026"
lastmod: '2024-03-09T21:06:05.252955-07:00'
model: gpt-4-0125-preview
summary: "H\xE5ndtering av feil i Dart handler om \xE5 forutse og h\xE5ndtere unntak\
  \ som oppst\xE5r under programkj\xF8ring for \xE5 forbedre p\xE5liteligheten og\
  \ brukervennligheten.\u2026"
title: "H\xE5ndtering av feil"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Håndtering av feil i Dart handler om å forutse og håndtere unntak som oppstår under programkjøring for å forbedre påliteligheten og brukervennligheten. Programmerere implementerer feilhåndtering for å forhindre krasj og gi meningsfull tilbakemelding til brukerne, noe som sikrer en jevnere, tryggere applikasjonsopplevelse.

## Hvordan:
Dart støtter to typer feil: *kompileringstid*-feil og *kjøretid*-feil. Kompileringstid-feil oppdages av Dart-analysatoren før koden kjører, mens kjøretid-feil, eller unntak, oppstår under utførelse. Slik håndterer du unntak i Dart:

### Try-Catch
Bruk `try-catch` for å fange opp unntak og forhindre at de krasjer applikasjonen din:

```dart
try {
  var result = 100 ~/ 0; // Forsøker divisjon med null, kaster et unntak
} catch (e) {
  print('Fanget et unntak: $e'); // Håndterer unntaket
}
```
Eksempelutskrift: `Fanget et unntak: IntegerDivisionByZeroException`

### Spesifikt Unntak
For å håndtere spesifikke unntak, nevn unntaket etter `catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Kan ikke dele på null.'); // Håndterer spesifikt deling med null-unntak
}
```
Eksempelutskrift: `Kan ikke dele på null.`

### Stakksporing
For å få en stakksporing for feilsøking, bruk en andre parameter i catch-blokken:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Unntak: $e');
  print('Stakksporing: $s'); // Skriver ut stakksporing for feilsøking
}
```

### Til slutt
Bruk `finally` for å utføre kode etter try/catch, uavhengig av om et unntak ble kastet:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Fanget et unntak: $e');
} finally {
  print('Dette blir alltid utført.'); // Oppryddingskode eller siste steg
}
```
Eksempelutskrift:
```
Fanget et unntak: IntegerDivisionByZeroException
Dette blir alltid utført.
```

### Tredjepartsbiblioteker
Selv om Darts kjernelibrary er robust for feilhåndtering, kan du også bruke tredjepartspakker som `dartz` for funksjonell programmering som innfører konsepter som `Either` og `Option` som kan brukes for feilhåndtering. Her er et eksempel på bruk av `dartz` for feilhåndtering:

1. Legg `dartz` til din `pubspec.yaml`-fil under avhengigheter:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Bruk `Either` for å håndtere feil på en nådig måte i Dart-koden din:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('Kan ikke dele på null.');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Feil: $left'), 
    (right) => print('Resultat: $right')
  );
}
```
Eksempelutskrift: `Feil: Kan ikke dele på null.`

`Left`-delen representerer vanligvis feilen, og `Right`-delen representerer suksess. Dette mønsteret tillater håndtering av feil på en mer funksjonell måte, og tilbyr klarhet og kontroll over feilhåndtering.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:12.037695-07:00
description: "\xC5 skrive til standardfeil (stderr) i Dart handler om \xE5 sende feilmeldinger\
  \ og diagnostikk til en separat str\xF8m, som er forskjellig fra standardutdata\u2026"
lastmod: '2024-03-11T00:14:14.035171-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive til standardfeil (stderr) i Dart handler om \xE5 sende feilmeldinger\
  \ og diagnostikk til en separat str\xF8m, som er forskjellig fra standardutdata\u2026"
title: Skriving til standard feil
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive til standardfeil (stderr) i Dart handler om å sende feilmeldinger og diagnostikk til en separat strøm, som er forskjellig fra standardutdata (stdout). Programmører gjør dette for å skille mellom normalt programutdata og feil- eller advarselsmeldinger, noe som tillater enklere feilsøking og logging.

## Hvordan:

I Dart er det enkelt å skrive til stderr ved bruk av `stderr`-objektet som er tilgjengelig i `dart:io`. Her er et grunnleggende eksempel:

```dart
import 'dart:io';

void main() {
  stderr.writeln('Dette er en feilmelding.');
}
```

Utdata ved kjøring:
```
Dette er en feilmelding.
```
Denne meldingen sendes til stderr-strømmen, som vanligvis vises i konsollen eller terminalen.

For å demonstrere mer kompleksitet, som for eksempel logging av et unntak, tillater Darts rike sett av funksjoner for konsis og effektiv feilhåndtering:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Simuler en operasjon som kan kaste
    throw Exception('Noe gikk galt!');
  } catch (e) {
    stderr.writeln('Feil: $e');
  }
}

void main() {
  riskyOperation();
}
```

Utdata ved kjøring:
```
Feil: Exception: Noe gikk galt!
```

Dette mønsteret er spesielt nyttig for applikasjoner som trenger å skille normale logger fra feillogger, noe som gjør det lettere å overvåke og feilsøke applikasjoner.

Selv om Darts standardbibliotek er ganske omfattende, trenger mange programmer ikke tredjepartsbiblioteker for å skrive til stderr. Imidlertid, hvis din applikasjon trenger mer sofistikerte loggingsmuligheter (f.eks. til filer, over nettverket, formatering), er `logging`-pakken et populært valg. Her er en kjapp titt på bruk av `logging` for feil:

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
  logger.severe('Alvorlig feil: Noe betydelig dårlig skjedde.');
}
```

Utdata ved kjøring:
```
SEVERE: 2023-04-01 00:00:00.000: Alvorlig feil: Noe betydelig dårlig skjedde.
```

Denne metoden tilbyr en høyere grad av tilpasning og kontroll over hva som blir logget som en feil og hvordan det formateres, noe som kan være veldig nyttig i større, mer komplekse applikasjoner.

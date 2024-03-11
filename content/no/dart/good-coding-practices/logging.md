---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:27.090905-07:00
description: "Logging i Dart refererer til prosessen med \xE5 registrere ulike niv\xE5\
  er av informasjon under utf\xF8relsen av et program. Programmerere gj\xF8r dette\
  \ for \xE5\u2026"
lastmod: '2024-03-11T00:14:14.023736-06:00'
model: gpt-4-0125-preview
summary: "Logging i Dart refererer til prosessen med \xE5 registrere ulike niv\xE5\
  er av informasjon under utf\xF8relsen av et program. Programmerere gj\xF8r dette\
  \ for \xE5\u2026"
title: Logging
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Logging i Dart refererer til prosessen med å registrere ulike nivåer av informasjon under utførelsen av et program. Programmerere gjør dette for å overvåke programvarens oppførsel, feilsøke problemer og analysere ytelse. Dette gjør det lettere å vedlikeholde og forbedre applikasjonen over tid.

## Hvordan:

Dart inkluderer en enkel loggingsmekanisme gjennom `dart:developer`-biblioteket. For mer sofistikerte loggingsbehov, vender programmerere ofte til tredjepartsbiblioteker som `logger` og `log4dart`.

### Bruke `dart:developer`
Dette er passende for grunnleggende logging, spesielt under utvikling:

```dart
import 'dart:developer';

void main() {
  log('Dette er en feilsøkingslogg-melding.');
}
```

Utdata:
```
Dette er en feilsøkingslogg-melding.
```

### Bruke `logger`-pakken
For en mer omfattende løsning, tilbyr `logger`-pakken ulike nivåer av logging (f.eks. info, advarsel, feil) og kan formateres på en mer lesbar måte.

Først, legg til `logger`-avhengigheten i din `pubspec.yaml`-fil:

```yaml
dependencies:
  logger: ^1.0.0
```

Deretter bruker du den slik:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Dette er en feilsøkingsmelding");
  logger.w("Dette er en advarselsmelding");
  logger.e("Dette er en feilmelding");
}
```

Et eksempelutdata kan se slik ut, med hver meldingstype formatert forskjellig for enkel identifikasjon:

```
💬 Dette er en feilsøkingsmelding
⚠️ Dette er en advarselsmelding
❗️ Dette er en feilmelding
```

### Bruke `log4dart`-pakken
For applikasjoner som krever konfigurasjonsbasert logging (liknende Log4j), tilbyr `log4dart` en kjent tilnærming. Det er spesielt hendig for storskala applikasjoner.

Sørg for at du inkluderer `log4dart` i din `pubspec.yaml`:

```yaml
dependencies:
  log4dart: ^2.0.0
```

Et enkelt brukseksempel:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("Feilsøker MyApp");
  logger.info("Informasjonsmelding");
}
```

Utdata:

```
DEBUG: Feilsøker MyApp
INFO: Informasjonsmelding
```

Hver av disse metodene tilbyr et forskjellig nivå av fleksibilitet og kompleksitet, fra enkle feilsøkingsmeldinger til omfattende, konfigurerbar logging tilpasset behovene til komplekse applikasjoner.

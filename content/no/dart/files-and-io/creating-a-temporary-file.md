---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:14.069796-07:00
description: "\xC5 opprette en midlertidig fil i Dart inneb\xE6rer \xE5 generere en\
  \ fil som er ment for korttidsbruk, hovedsakelig for scenarioer som caching av data,\u2026"
lastmod: '2024-03-13T22:44:40.508873-06:00'
model: gpt-4-0125-preview
summary: "\xC5 opprette en midlertidig fil i Dart inneb\xE6rer \xE5 generere en fil\
  \ som er ment for korttidsbruk, hovedsakelig for scenarioer som caching av data,\u2026"
title: Oppretting av en midlertidig fil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil i Dart innebærer å generere en fil som er ment for korttidsbruk, hovedsakelig for scenarioer som caching av data, midlertidig lagring for filbehandling eller oppbevaring av informasjon som er for sensitiv til å beholde lenge. Programmerere gjør dette for å håndtere data som ikke trenger permanent lagring, dermed forbedrer de ytelsen og opprettholder datahygiene.

## Hvordan:
Dart's `dart:io` biblioteket legger til rette for opprettelsen av midlertidige filer gjennom `Directory` klassen. Her er en enkel måte å opprette en midlertidig fil og skrive noe innhold i den:

```dart
import 'dart:io';

Future<void> main() async {
  // Opprett en midlertidig mappe (systemspesifikk plassering)
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // Opprett en midlertidig fil innenfor den mappen
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Skriv noe innhold til den midlertidige filen
  await tempFile.writeAsString('Dette er noe midlertidig innhold');

  print('Midlertidig fil opprettet: ${tempFile.path}');

  // Eksempel på utdata: Midlertidig fil opprettet: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### Bruk av tredjepartsbibliotek: `path_provider`

For applikasjoner (spesielt mobilapper med Flutter), kan du ønske å opprette midlertidige filer på en mer enhetlig og håndterbar måte. `path_provider` pakken kan hjelpe deg med å finne den riktige midlertidige mappen på tvers av forskjellige platformer (iOS, Android, osv.).

Først, legg til `path_provider` i din `pubspec.yaml` under dependencies:

```yaml
dependencies:
  path_provider: ^2.0.9
```

Og her er hvordan du kan bruke den til å opprette en midlertidig fil:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // Få den midlertidige mappen
  final Directory tempDir = await getTemporaryDirectory();

  // Opprett en midlertidig fil innenfor den mappen
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Skriv noe innhold til den midlertidige filen
  await tempFile.writeAsString('Dette er noe midlertidig innhold med path_provider');

  print('Midlertidig fil opprettet med path_provider: ${tempFile.path}');

  // Eksempel på utdata: Midlertidig fil opprettet med path_provider: /tmp/my_temp_file.txt (sti kan variere etter plattform)
}
```

Disse kodestykkene illustrerer opprettelsen og interaksjonen med midlertidige filer i Dart, og gir en grei og praktisk tilnærming til datalagring for korttidsformål.

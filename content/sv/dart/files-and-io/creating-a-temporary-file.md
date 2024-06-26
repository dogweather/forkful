---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:27.889129-07:00
description: "Hur man g\xF6r: Darts `dart:io`-bibliotek underl\xE4ttar skapandet av\
  \ tempor\xE4ra filer genom `Directory`-klassen. H\xE4r \xE4r ett enkelt s\xE4tt\
  \ att skapa en tempor\xE4r\u2026"
lastmod: '2024-03-13T22:44:37.633288-06:00'
model: gpt-4-0125-preview
summary: "Darts `dart:io`-bibliotek underl\xE4ttar skapandet av tempor\xE4ra filer\
  \ genom `Directory`-klassen."
title: "Skapa en tempor\xE4r fil"
weight: 21
---

## Hur man gör:
Darts `dart:io`-bibliotek underlättar skapandet av temporära filer genom `Directory`-klassen. Här är ett enkelt sätt att skapa en temporär fil och skriva något innehåll till den:

```dart
import 'dart:io';

Future<void> main() async {
  // Skapa en temporär katalog (platsberoende på systemet)
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // Skapa en temporär fil inom den katalogen
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Skriv något innehåll till den temporära filen
  await tempFile.writeAsString('Detta är tillfälligt innehåll');

  print('Temporär fil skapad: ${tempFile.path}');

  // Exempel på utskrift: Temporär fil skapad: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### Använda ett tredjepartbibliotek: `path_provider`
För applikationer (speciellt mobilappar med Flutter) kanske du vill skapa temporära filer på ett mer enhetligt och hanterbart sätt. `path_provider`-paketet kan hjälpa dig att hitta den korrekta tillfälliga katalogen på olika plattformar (iOS, Android osv.).

Först, lägg till `path_provider` i din `pubspec.yaml` under beroenden:

```yaml
dependencies:
  path_provider: ^2.0.9
```

Och så här kan du använda det för att skapa en temporär fil:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // Hämta den tillfälliga katalogen
  final Directory tempDir = await getTemporaryDirectory();

  // Skapa en temporär fil inom den katalogen
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Skriv något innehåll till den temporära filen
  await tempFile.writeAsString('Detta är tillfälligt innehåll med path_provider');

  print('Temporär fil skapad med path_provider: ${tempFile.path}');

  // Exempel på utskrift: Temporär fil skapad med path_provider: /tmp/my_temp_file.txt (sökvägen kan variera beroende på plattform)
}
```

Dessa kodsnuttar illustrerar hur du skapar och interagerar med temporära filer i Dart, vilket ger ett enkelt och praktiskt tillvägagångssätt för dataskydd vid kortsiktiga syften.

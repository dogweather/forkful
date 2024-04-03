---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:34.973151-07:00
description: "Hur man g\xF6r: Darts k\xE4rnbibliotek, `dart:io`, erbjuder de n\xF6\
  dv\xE4ndiga funktionerna f\xF6r att l\xE4sa textfiler synkront eller asynkront.\
  \ S\xE5 h\xE4r kan du g\xE5\u2026"
lastmod: '2024-03-13T22:44:37.631142-06:00'
model: gpt-4-0125-preview
summary: "Darts k\xE4rnbibliotek, `dart:io`, erbjuder de n\xF6dv\xE4ndiga funktionerna\
  \ f\xF6r att l\xE4sa textfiler synkront eller asynkront."
title: "L\xE4sa en textfil"
weight: 22
---

## Hur man gör:
Darts kärnbibliotek, `dart:io`, erbjuder de nödvändiga funktionerna för att läsa textfiler synkront eller asynkront. Så här kan du gå tillväga på båda sätten.

**Synkront:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // Läser filen synkront
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Fel vid läsning av filen: $e');
  }
}
```

**Asynkront:**

För att undvika att blockera programmet medan filen läses, vilket är särskilt användbart för stora filer eller responsiva applikationer:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Fel vid läsning av filen: $e');
  }
}
```

**Exempel på utdata:**

Om din textfil innehåller:

```
Hello, Dart!
```

Kommer båda ovanstående metoder att ge utdatan:

```
Hello, Dart!
```

**Att använda ett tredjepartsbibliotek:**

För ytterligare funktioner som förenklade filoperationer eller förbättrad felhantering kan du överväga att använda tredjepartsbibliotek såsom `package:file`. Dock, enligt min senaste uppdatering, är användningen av kärnpaketet `dart:io` direkt, som visas ovan, det vanligaste och mest okomplicerade sättet att läsa textfiler i Dart.

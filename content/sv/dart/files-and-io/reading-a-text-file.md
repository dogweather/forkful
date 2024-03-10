---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:34.973151-07:00
description: "Att l\xE4sa en textfil i Dart inneb\xE4r att man f\xE5r tillg\xE5ng\
  \ till och h\xE4mtar data fr\xE5n filer som \xE4r lagrade p\xE5 filsystemet. Programmerare\
  \ g\xF6r detta f\xF6r att\u2026"
lastmod: '2024-03-09T21:06:02.385177-07:00'
model: gpt-4-0125-preview
summary: "Att l\xE4sa en textfil i Dart inneb\xE4r att man f\xE5r tillg\xE5ng till\
  \ och h\xE4mtar data fr\xE5n filer som \xE4r lagrade p\xE5 filsystemet. Programmerare\
  \ g\xF6r detta f\xF6r att\u2026"
title: "L\xE4sa en textfil"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil i Dart innebär att man får tillgång till och hämtar data från filer som är lagrade på filsystemet. Programmerare gör detta för att hantera indata, konfigurationsinställningar eller läsa datamängder, vilket gör det till en grundläggande operation för många applikationer som sträcker sig från enkla skript till komplexa appar.

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

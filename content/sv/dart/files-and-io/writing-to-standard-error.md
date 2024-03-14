---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:03.920330-07:00
description: "Att skriva till standardfel (stderr) i Dart handlar om att skicka felmeddelanden\
  \ och diagnostik till en separat str\xF6m, skild fr\xE5n standardutdata (stdout).\u2026"
lastmod: '2024-03-13T22:44:37.630072-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i Dart handlar om att skicka felmeddelanden\
  \ och diagnostik till en separat str\xF6m, skild fr\xE5n standardutdata (stdout).\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standardfel (stderr) i Dart handlar om att skicka felmeddelanden och diagnostik till en separat ström, skild från standardutdata (stdout). Programmerare gör detta för att skilja mellan normal programutdata och fel eller varningsmeddelanden, vilket underlättar felsökning och loggning.

## Hur man gör:

I Dart är det enkelt att skriva till stderr genom att använda `stderr`-objektet som finns tillgängligt i `dart:io`. Här är ett grundläggande exempel:

```dart
import 'dart:io';

void main() {
  stderr.writeln('Detta är ett felmeddelande.');
}
```

Utskrift vid körning:
```
Detta är ett felmeddelande.
```
Detta meddelande skickas till stderr-strömmen, som vanligtvis visas i konsolen eller terminalen.

För att visa mer komplexitet, som att logga ett undantag, tillåter Darts rika uppsättning av funktioner koncis och effektiv felsökning:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Simulera en operation som kan kasta
    throw Exception('Något gick fel!');
  } catch (e) {
    stderr.writeln('Fel: $e');
  }
}

void main() {
  riskyOperation();
}
```

Utskrift vid körning:
```
Fel: Exception: Något gick fel!
```

Detta mönster är särskilt användbart för applikationer som behöver separera normala loggar från felloggar, vilket gör det lättare att övervaka och felsöka applikationer.

Även om Darts standardbibliotek är ganska omfattande, kräver många program inte tredjepartsbibliotek för att skriva till stderr. Om din applikation dock behöver mer avancerade loggningsmöjligheter (t.ex. till filer, över nätverket, formattering), är `logging`-paketet ett populärt val. Här är en snabb titt på att använda `logging` för fel:

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
  logger.severe('Allvarligt Fel: Något avsevärt dåligt hände.');
}
```

Utskrift vid körning:
```
SEVERE: 2023-04-01 00:00:00.000: Allvarligt Fel: Något avsevärt dåligt hände.
```

Denna metod erbjuder en högre grad av anpassning och kontroll över vad som loggas som ett fel och hur det formateras, vilket kan vara mycket användbart i större, mer komplexa applikationer.

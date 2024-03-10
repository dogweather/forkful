---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:04.362191-07:00
description: "Loggning i Dart avser processen att spela in olika niv\xE5er av information\
  \ under k\xF6rningen av ett program. Programmerare g\xF6r detta f\xF6r att \xF6\
  vervaka\u2026"
lastmod: '2024-03-09T21:06:02.375329-07:00'
model: gpt-4-0125-preview
summary: "Loggning i Dart avser processen att spela in olika niv\xE5er av information\
  \ under k\xF6rningen av ett program. Programmerare g\xF6r detta f\xF6r att \xF6\
  vervaka\u2026"
title: Loggning
---

{{< edit_this_page >}}

## Vad & Varför?

Loggning i Dart avser processen att spela in olika nivåer av information under körningen av ett program. Programmerare gör detta för att övervaka mjukvarans beteende, felsöka problem och analysera prestanda, vilket gör det enklare att underhålla och förbättra applikationen över tid.

## Hur man gör:

Dart inkluderar en enkel loggningsmekanism genom biblioteket `dart:developer`. För mer sofistikerade loggningsbehov vänder sig programmerare ofta till tredjepartsbibliotek som `logger` och `log4dart`.

### Att använda `dart:developer`
Detta är lämpligt för grundläggande loggning, särskilt under utveckling:

```dart
import 'dart:developer';

void main() {
  log('Det här är ett felsökningsloggmeddelande.');
}
```

Utskrift:
```
Det här är ett felsökningsloggmeddelande.
```

### Att använda paketet `logger`
För en mer heltäckande lösning erbjuder paketet `logger` olika nivåer av loggning (t.ex. info, varning, fel) och kan formateras på ett mer läsligt sätt.

Lägg först till beroendet `logger` i din `pubspec.yaml`-fil:

```yaml
dependencies:
  logger: ^1.0.0
```

Använd den sedan på följande sätt:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Det här är ett felsökningsmeddelande");
  logger.w("Det här är ett varningsmeddelande");
  logger.e("Det här är ett felmeddelande");
}
```

Exempel på utskrift kan se ut så här, med varje meddelandetyp formaterad annorlunda för enkel identifiering:

```
💬 Det här är ett felsökningsmeddelande
⚠️ Det här är ett varningsmeddelande
❗️ Det här är ett felmeddelande
```

### Att använda paketet `log4dart`
För applikationer som kräver konfigurationsbaserad loggning (liknande Log4j) erbjuder `log4dart` ett bekant tillvägagångssätt. Det är särskilt praktiskt för storskaliga applikationer.

Se till att du inkluderar `log4dart` i din `pubspec.yaml`:

```yaml
dependencies:
  log4dart: ^2.0.0
```

Ett enkelt exempel på användning:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("Felsöker MyApp");
  logger.info("Informativt meddelande");
}
```

Utskrift:

```
DEBUG: Felsöker MyApp
INFO: Informativt meddelande
```

Var och en av dessa metoder erbjuder en annan nivå av flexibilitet och komplexitet, från enkla felsökningsmeddelanden till omfattande, konfigurerbar loggning som passar behoven hos komplexa applikationer.

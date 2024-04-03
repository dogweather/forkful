---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:54.793249-07:00
description: "Hvordan gj\xF8re det: Dart tilbyr `DateTime`-klassen for h\xE5ndtering\
  \ av datoer og tider, og `intl`-pakken for formatering. F\xF8rst, s\xF8rg for at\
  \ du har\u2026"
lastmod: '2024-03-13T22:44:40.499954-06:00'
model: gpt-4-0125-preview
summary: "Dart tilbyr `DateTime`-klassen for h\xE5ndtering av datoer og tider, og\
  \ `intl`-pakken for formatering."
title: Konvertere en dato til en streng
weight: 28
---

## Hvordan gjøre det:
Dart tilbyr `DateTime`-klassen for håndtering av datoer og tider, og `intl`-pakken for formatering. Først, sørg for at du har `intl`-pakken ved å legge til `intl: ^0.17.0` (eller den nyeste versjonen) i din `pubspec.yaml`-fil.

### Bruke Darts kjernebibliotek
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Utdata: 2023-4-12 (for eksempel, dette avhenger av gjeldende dato)
```

Dette eksemplet lager direkte en streng fra `DateTime`-egenskapene.

### Bruke `intl`-pakken
Først, importer pakken:

```dart
import 'package:intl/intl.dart';
```

Deretter, formater datoen:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Utdata: 2023-04-12
```

`intl`-pakken tillater mye mer kompleks formatering enkelt, inkludert lokalt-spesifikke formater:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Utdata: April 12, 2023
```

Disse eksemplene viser enkle, men kraftfulle måter å konvertere og formatere datoer til strenger i Dart, enten ved å bruke Darts kjernefunksjonalitet eller ved å utnytte `intl`-pakken for mer avanserte formateringsalternativer.

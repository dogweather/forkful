---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:56.678514-07:00
description: "Att konvertera ett datum till en str\xE4ng i Dart \xE4r en vanlig uppgift\
  \ n\xE4r du beh\xF6ver visa datum- och tidsinformation i ett l\xE4ttl\xE4st format,\
  \ eller n\xE4r du\u2026"
lastmod: '2024-03-09T21:06:02.379809-07:00'
model: gpt-4-0125-preview
summary: "Att konvertera ett datum till en str\xE4ng i Dart \xE4r en vanlig uppgift\
  \ n\xE4r du beh\xF6ver visa datum- och tidsinformation i ett l\xE4ttl\xE4st format,\
  \ eller n\xE4r du\u2026"
title: "Att konvertera ett datum till en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera ett datum till en sträng i Dart är en vanlig uppgift när du behöver visa datum- och tidsinformation i ett lättläst format, eller när du tänker serialisera data för lagring eller överföring. Denna process möjliggör enkel representation och manipulation av datum-tidsvärden i ett format som är både begripligt och kan anpassas beroende på användningsfall.

## Hur:

Dart tillhandahåller klassen `DateTime` för hantering av datum och tider, samt paketet `intl` för formatering. Se först till att du har paketet `intl` genom att lägga till `intl: ^0.17.0` (eller den senaste versionen) i din `pubspec.yaml`-fil.

### Använda Darts Kärnbibliotek

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Utdata: 2023-4-12 (till exempel, detta beror på det aktuella datumet)
```

Detta exempel bygger direkt en sträng från `DateTime`-objektets egenskaper.

### Använda `intl`-paketet

Importera först paketet:

```dart
import 'package:intl/intl.dart';
```

Formatera sedan datumet:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Utdata: 2023-04-12
```

Paketet `intl` möjliggör mycket mer komplex formatering på ett enkelt sätt, inklusive lokal-specifika format:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Utdata: April 12, 2023
```

Dessa exempel visar enkla men kraftfulla sätt att konvertera och formatera datum till strängar i Dart, antingen genom att använda Darts kärnfunktioner eller genom att utnyttja `intl`-paketet för mer avancerade formateringsalternativ.

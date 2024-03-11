---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:03.729636-07:00
description: "Att h\xE4mta det aktuella datumet i Dart inneb\xE4r att man fr\xE5gar\
  \ systemet efter det aktuella datumet och tiden. Denna funktionalitet anv\xE4nds\
  \ vanligtvis i\u2026"
lastmod: '2024-03-11T00:14:10.955880-06:00'
model: gpt-4-0125-preview
summary: "Att h\xE4mta det aktuella datumet i Dart inneb\xE4r att man fr\xE5gar systemet\
  \ efter det aktuella datumet och tiden. Denna funktionalitet anv\xE4nds vanligtvis\
  \ i\u2026"
title: "H\xE4mta aktuellt datum"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet i Dart innebär att man frågar systemet efter det aktuella datumet och tiden. Denna funktionalitet används vanligtvis i applikationer för funktioner som att tidsstämpla händelser, visa det aktuella datumet för användare eller beräkna varaktigheter. Att veta hur man effektivt hämtar och manipulerar det aktuella datumet är grundläggande för schemaläggning, loggning och tidskänsliga funktioner.

## Hur man gör:
Darts kärnbibliotek ger direkt åtkomst till det aktuella datumet och tiden genom `DateTime`-klassen. Här är ett grundläggande exempel för att hämta det aktuella datumet:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Exempelutskrift: 2023-04-12 10:00:00.000
}
```

Om du bara behöver datumdelen (år, månad, dag) kan du formatera `DateTime`-objektet:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Exempelutskrift: 2023-04-12
}
```

Dart inkluderar inte ett inbyggt bibliotek för mer komplex datumformatering, men du kan använda `intl`-paketet för detta ändamål. Först, lägg till paketet i din `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Därefter kan du enkelt formatera datum:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Exempelutskrift: 2023-04-12
}
```

För mer avancerade formateringsalternativ, utforska `DateFormat`-klassen som erbjuds av `intl`-paketet, som stöder ett brett spektrum av mönster och lokaler.

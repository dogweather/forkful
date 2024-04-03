---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:40.500839-07:00
description: "Hur man g\xF6r: Darts k\xE4rnbibliotek f\xF6renklar datumtolkning genom\
  \ klassen `DateTime`. F\xF6r enkla fall d\xE4r du k\xE4nner till formatet p\xE5\
  \ datumstr\xE4ngen kan du\u2026"
lastmod: '2024-03-13T22:44:37.622331-06:00'
model: gpt-4-0125-preview
summary: "Darts k\xE4rnbibliotek f\xF6renklar datumtolkning genom klassen `DateTime`."
title: "Analys av ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:
Darts kärnbibliotek förenklar datumtolkning genom klassen `DateTime`. För enkla fall där du känner till formatet på datumsträngen kan du använda metoden `DateTime.parse()`. Men, för mer komplexa scenarion eller när man har att göra med flera format, blir paketet `intl`, specifikt klassen `DateFormat`, ovärderligt.

### Använda Dart Core Library:
```dart
void main() {
  // Använda DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Använda `intl`-paketet:
Lägg först till `intl`-paketet i din `pubspec.yaml`-fil:
```yaml
dependencies:
  intl: ^0.17.0
```
Importera sedan paketet och använd `DateFormat` för att tolka:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "Oktober 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
`intl`-paketet erbjuder robusta alternativ för datumtolkning, vilket möjliggör smidig hantering av olika internationella datumformat.

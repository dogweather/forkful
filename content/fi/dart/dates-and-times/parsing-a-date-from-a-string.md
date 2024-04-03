---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:43.967744-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta Dartissa\
  \ tarkoittaa p\xE4iv\xE4m\xE4\xE4rien ja kellonaikojen tekstiesitysten muuntamista\
  \ `DateTime`-objektiksi. T\xE4m\xE4 toiminto\u2026"
lastmod: '2024-03-13T22:44:56.281173-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta Dartissa tarkoittaa\
  \ p\xE4iv\xE4m\xE4\xE4rien ja kellonaikojen tekstiesitysten muuntamista `DateTime`-objektiksi."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
weight: 30
---

## Miten:
Dartin ydinkirjasto yksinkertaistaa päivämäärän jäsentämistä `DateTime`-luokan avulla. Suoraviivaisissa tapauksissa, joissa tiedät päivämäärämerkkijonon muodon, voit käyttää `DateTime.parse()`-metodia. Kuitenkin monimutkaisemmissa skenaarioissa tai kun käsitellään useita muotoja, `intl`-paketti, erityisesti `DateFormat`-luokka, on korvaamaton.

### Käyttäen Dart-ydinkirjastoa:
```dart
void main() {
  // Käyttäen DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Käyttäen `intl`-pakettia:
Lisää ensin `intl`-paketti `pubspec.yaml`-tiedostoosi:
```yaml
dependencies:
  intl: ^0.17.0
```
Tämän jälkeen, tuo paketti ja käytä `DateFormat`ia jäsentämiseen:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
`intl`-paketti tarjoaa robustit vaihtoehdot päivämäärän jäsentämiseen, mahdollistaen erilaisten kansainvälisten päivämäärämuotojen saumattoman käsittelyn.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:43.967744-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta Dartissa\
  \ tarkoittaa p\xE4iv\xE4m\xE4\xE4rien ja kellonaikojen tekstiesitysten muuntamista\
  \ `DateTime`-objektiksi. T\xE4m\xE4 toiminto\u2026"
lastmod: '2024-03-11T00:14:30.212972-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta Dartissa tarkoittaa\
  \ p\xE4iv\xE4m\xE4\xE4rien ja kellonaikojen tekstiesitysten muuntamista `DateTime`-objektiksi.\
  \ T\xE4m\xE4 toiminto\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta Dartissa tarkoittaa päivämäärien ja kellonaikojen tekstiesitysten muuntamista `DateTime`-objektiksi. Tämä toiminto on olennainen sovelluksille, jotka käsittelevät aikataulutusta, data-analyysiä tai mitä tahansa ominaisuutta, joka vaatii päivämäärän käsittelyä, varmistaen, että päivämäärään liittyvät tiedot ymmärretään oikein ja käsitellään ohjelmassa kunnolla.

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

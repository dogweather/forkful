---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:56.301985-07:00
description: "Lainausmerkkien poistaminen merkkijonosta Dart-kieless\xE4 tarkoittaa\
  \ kaksois- (\") tai yksitt\xE4isten (') lainausmerkkien karsimista merkkijonon alusta\
  \ ja\u2026"
lastmod: '2024-03-11T00:14:30.188909-06:00'
model: gpt-4-0125-preview
summary: "Lainausmerkkien poistaminen merkkijonosta Dart-kieless\xE4 tarkoittaa kaksois-\
  \ (\") tai yksitt\xE4isten (') lainausmerkkien karsimista merkkijonon alusta ja\u2026"
title: Lainausmerkkien poistaminen merkkijonosta
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lainausmerkkien poistaminen merkkijonosta Dart-kielessä tarkoittaa kaksois- (") tai yksittäisten (') lainausmerkkien karsimista merkkijonon alusta ja lopusta, joka on hyödyllistä tietojen puhdistuksessa tai merkkijonojen valmistelussa jatkokäsittelyä varten. Ohjelmoijat tekevät tämän normalisoidakseen datan syötteitä, varmistaakseen yhtenäisyyden datan tallennuksessa, tai kun ollaan vuorovaikutuksessa API:en kanssa, jotka saattavat palauttaa datan lainatussa muodossa.

## Miten:
Dart tarjoaa suoraviivaisia tapoja poistaa lainausmerkit merkkijonosta käyttäen sisäänrakennettuja merkkijonometodeja ilman kolmannen osapuolen kirjastoja.

### Esimerkki 1: Käyttäen `replaceFirst` ja `replaceAll`
Jos käsittelet merkkijonoja, jotka alkavat ja päättyvät lainausmerkeillä, voit käyttää `replaceFirst` ja `replaceAll` metodeja niiden poistamiseen.

```dart
String quotedString = '"Hei, Maailma!"';
String singleQuotedString = '\'Dart Ohjelmointi\'';

// Kahdensijaiset lainausmerkit pois
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Tuloste: Hei, Maailma!

// Yksittäiset lainausmerkit pois
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Tuloste: Dart Ohjelmointi
```

### Esimerkki 2: Käyttäen `substring`
Tämä metodi on hyödyllinen, kun olet varma, että lainausmerkit ovat aivan merkkijonon alussa ja lopussa.

```dart
String quotedString = '"Flutter Kehitys"';
// Tarkista, että se alkaa ja päättyy lainausmerkeillä ennen poistamista välttääksesi virheitä
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Tuloste: Flutter Kehitys
```

### Esimerkki 3: Räätälöity Laajennusmetodi
Lisää uudelleenkäytettävyyttä varten, erityisesti jos projektisi sisältää toistuvaa lainausmerkkien poistoa, harkitse räätälöidyn laajennuksen luomista `String`-tyypille.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"Tämä on Dart"';
  String singleQuoted = '\'Tämä on mahtavaa\'';
  print(doubleQuoted.unquote()); // Tuloste: Tämä on Dart
  print(singleQuoted.unquote()); // Tuloste: Tämä on mahtavaa
}
```

Näiden lähestymistapojen avulla sinun pitäisi pystyä tehokkaasti poistamaan lainausmerkit merkkijonoista Dart-kielessä, parantaen datan käsittelyä ja valmistelua työnkulkujasi.

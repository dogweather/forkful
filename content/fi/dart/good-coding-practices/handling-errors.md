---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:43.690219-07:00
description: "Virheiden k\xE4sittely Dartissa tarkoittaa ohjelman suorituksen aikana\
  \ ilmenevien poikkeusten ennakointia ja hallintaa luotettavuuden ja k\xE4ytett\xE4\
  vyyden\u2026"
lastmod: '2024-03-09T21:06:20.187773-07:00'
model: gpt-4-0125-preview
summary: "Virheiden k\xE4sittely Dartissa tarkoittaa ohjelman suorituksen aikana ilmenevien\
  \ poikkeusten ennakointia ja hallintaa luotettavuuden ja k\xE4ytett\xE4vyyden\u2026"
title: "Virheiden k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Virheiden käsittely Dartissa tarkoittaa ohjelman suorituksen aikana ilmenevien poikkeusten ennakointia ja hallintaa luotettavuuden ja käytettävyyden parantamiseksi. Ohjelmoijat toteuttavat virheidenhallintaa estääkseen sovelluksen kaatumisen ja tarjotakseen käyttäjille merkityksellistä palautetta, varmistaen sujuvamman ja turvallisemman sovelluskokemuksen.

## Kuinka:
Dart tukee kahta virhetyyppiä: *käännösaikaiset* virheet ja *suoritusaikaiset* virheet. Käännösaikaiset virheet havaitsee Dart-analysaattori ennen koodin suoritusta, kun taas suoritusaikaiset virheet eli poikkeukset ilmenevät suorituksen aikana. Tässä on, miten käsitellä poikkeuksia Dartissa:

### Try-Catch
Käytä `try-catch`-lohkoa poikkeusten nappaamiseen ja estääkseen niitä kaatamasta sovellustasi:

```dart
try {
  var tulos = 100 ~/ 0; // Yrittää jakaa nollalla, heittää poikkeuksen
} catch (e) {
  print('Napattiin poikkeus: $e'); // Käsittelee poikkeuksen
}
```
Esimerkkitulo: `Napattiin poikkeus: IntegerDivisionByZeroException`

### Tietty Poikkeus
Tietyt poikkeukset voidaan käsitellä mainitsemalla poikkeus `catch`-jälkeen:

```dart
try {
  var tulos = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Ei voi jakaa nollalla.'); // Käsittelee nimenomaan nollalla jaon poikkeukset
}
```
Esimerkkitulo: `Ei voi jakaa nollalla.`

### Pinorakenne
Debuggausta varten voit saada pinorakenteen käyttämällä toista parametria catch-lohkossa:

```dart
try {
  var tulos = 100 ~/ 0;
} catch (e, s) {
  print('Poikkeus: $e');
  print('Pinorakenne: $s'); // Tulostaa pinorakenteen debuggausta varten
}
```

### Finally
Käytä `finally`-lohkoa suorittaaksesi koodia try/catch-lohkon jälkeen, riippumatta siitä, heitettiinkö poikkeus:

```dart
try {
  var tulos = 100 ~/ 0;
} catch (e) {
  print('Napattiin poikkeus: $e');
} finally {
  print('Tämä suoritetaan aina.'); // Siivouskoodi tai lopulliset vaiheet
}
```
Esimerkkitulo:
```
Napattiin poikkeus: IntegerDivisionByZeroException
Tämä suoritetaan aina.
```

### Kolmannen Osapuolen Kirjastot
Vaikka Dartin ydinkirjasto on vankka virheiden käsittelyyn, voit myös käyttää kolmannen osapuolen paketteja, kuten `dartz` funktionaaliseen ohjelmointiin, joka esittelee konsepteja kuten `Either` ja `Option`, joita voidaan käyttää virheiden käsittelyssä. Tässä on esimerkki käyttäen `dartz`-pakettia virheiden käsittelyssä:

1. Lisää `dartz` `pubspec.yaml`-tiedostoosi riippuvuuksiin:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Käytä `Either`-rakennetta käsittelämään virheitä sulavasti Dart-koodissasi:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int jaettava, int jakaja) {
  if (jakaja == 0) {
    return Left('Ei voi jakaa nollalla.');
  } else {
    return Right(jaettava ~/ jakaja);
  }
}

void main() {
  final tulos = divide(100, 0);
  tulos.fold(
    (vasen) => print('Virhe: $vasen'), 
    (oikea) => print('Tulos: $oikea')
  );
}
```
Esimerkkitulo: `Virhe: Ei voi jakaa nollalla.`

`Left` osa yleensä edustaa virhettä, ja `Right` osa edustaa onnistumista. Tämä malli mahdollistaa virheiden käsittelyn funktionaalisemmalla tavalla ja tarjoaa selkeyttä ja hallintaa virheiden hallinnassa.

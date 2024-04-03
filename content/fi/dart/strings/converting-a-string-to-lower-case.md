---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:03.921177-07:00
description: "Merkkijonon muuttaminen pieniksi kirjaimiksi on perusoperaatio, joka\
  \ k\xE4sitt\xE4\xE4 kaikkien annetun merkkijonon merkkien muuttamisen niiden pienikirjaimisiin\u2026"
lastmod: '2024-03-13T22:44:56.256507-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon muuttaminen pieniksi kirjaimiksi on perusoperaatio, joka k\xE4\
  sitt\xE4\xE4 kaikkien annetun merkkijonon merkkien muuttamisen niiden pienikirjaimisiin\
  \ vastineisiin."
title: Merkkijonon muuttaminen pieniksi kirjaimiksi
weight: 4
---

## Mikä ja Miksi?

Merkkijonon muuttaminen pieniksi kirjaimiksi on perusoperaatio, joka käsittää kaikkien annetun merkkijonon merkkien muuttamisen niiden pienikirjaimisiin vastineisiin. Ohjelmoijat suorittavat tyypillisesti tämän operaation saavuttaakseen kirjainkoosta riippumattomat vertailut tai standardoidakseen tekstisyötteen edelleen käsittelyä varten, mikä tekee sovelluksista käyttäjäystävällisempiä ja tiedosta johdonmukaisempaa.

## Kuinka:

Dartissa voit muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä `String`-luokan tarjoamaa `toLowerCase()`-metodia. Tämä metodi palauttaa uuden merkkijonon, jossa kaikki suurkirjaimet on muutettu pieniksi kirjaimiksi. Katsotaan, miten tämä toimii yksinkertaisen esimerkin avulla:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Tuloste: hello, world!
}
```

Dart ei vaadi ulkoisia kirjastoja perus merkkijonon käsittelytehtäviin, mukaan lukien muuntaminen pieniksi kirjaimiksi, koska standardikirjaston `String`-luokka on varsin kattava. Kuitenkin monimutkaisempia manipulaatioita varten, jotka liittyvät kielikohtaisiin sääntöihin, saattaisit harkita `intl`-pakettia, joka tarjoaa kansainvälistämisen ja lokalisoinnin mahdollisuuksia, mukaan lukien kirjainkoon muuttaminen kielen mukaan:

Käyttääksesi `intl`, lisää se `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  intl: ^0.17.0
```

Sen jälkeen voit käyttää `toLocaleLowerCase()`-metodia muuttaaksesi merkkijonon pieniksi kirjaimiksi tietyillä kieli- ja alueasetuksilla:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Turkin kieli
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Tuloste: istanbul
  
  // Oletuskieli (en)
  print(originalString.toLowerCase()); // Tuloste: i̇stanbul
}
```

Tässä esimerkissä huomaa, kuinka Turkin kieliasetus käsittelee oikein pistettömän 'i':n, mikä osoittaa kielitietoisten muunnosten merkityksen kansainvälisissä sovelluksissa.

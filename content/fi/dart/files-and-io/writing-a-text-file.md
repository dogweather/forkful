---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:57.756466-07:00
description: "Tekstitiedoston kirjoittaminen Dartissa sis\xE4lt\xE4\xE4 tiedostojen\
  \ luomisen tai muokkaamisen levylle, jotta data saadaan tallennettua luettavaan\
  \ muotoon.\u2026"
lastmod: '2024-03-09T21:06:20.197857-07:00'
model: gpt-4-0125-preview
summary: "Tekstitiedoston kirjoittaminen Dartissa sis\xE4lt\xE4\xE4 tiedostojen luomisen\
  \ tai muokkaamisen levylle, jotta data saadaan tallennettua luettavaan muotoon.\u2026"
title: Tekstitiedoston kirjoittaminen
---

{{< edit_this_page >}}

## Mikä ja miksi?
Tekstitiedoston kirjoittaminen Dartissa sisältää tiedostojen luomisen tai muokkaamisen levylle, jotta data saadaan tallennettua luettavaan muotoon. Ohjelmoijat tekevät sen tallentaakseen sovelluksen tiedot, asetukset, lokit tai minkä tahansa tiedon, joka tulisi säilyä sovelluksen käyttökertojen välillä tai jakanakseen tietoa muiden sovellusten tai käyttäjien kanssa.

## Miten:
Dartin ydin kirjasto tarjoaa `dart:io` -paketin tiedostojen käsittelyä varten, mikä mahdollistaa tekstifilien kirjoittamisen ilman kolmannen osapuolen kirjastoja tarvetta. Tässä on yksinkertainen esimerkki tekstifilin kirjoittamisesta:

```dart
import 'dart:io';

void main() async {
  // Luo uusi tiedosto nimeltä 'example.txt' nykyiseen hakemistoon.
  var file = File('example.txt');
  
  // Kirjoita merkkijono tiedostoon.
  await file.writeAsString('Hei, Dart!');
  
  // Tarkista sisältö.
  print(await file.readAsString()); // Tuloste: Hei, Dart!
}
```

Käsittäessäsi suurempia tiedostoja tai datavirtoja, saatat mieluummin kirjoittaa sisältöä käyttäen `openWrite`, joka palauttaa `IOSink`:n ja mahdollistaa datan kirjoittamisen osioissa:

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // Kirjoita useita rivejä tiedostoon.
  sink
    ..writeln('Rivi 1: Nopea ruskea kettu hyppää laiskan koiran yli.')
    ..writeln('Rivi 2: Dart on mahtava!')
    ..close();

  // Odota, että sink sulkeutuu varmistaaksesi, että kaikki data on kirjoitettu tiedostoon.
  await sink.done;

  // Lue ja tulosta tiedoston sisältö varmistaaksesi
  print(await file.readAsString());
}
```

Tehdessäsi tarkempaa tiedostojen käsittelyä, mukaan lukien lisääminen tiedostoihin tai tavujen kirjoittaminen, saatat syventyä syvemmin `File` luokan metodeihin jotka `dart:io` tarjoaa. Lisäksi työskenneltäessä suuren mittakaavan tai monimutkaisempien projektien parissa, harkitse kirjastoja kuten `path` tiedostopolkujen käsittelyyn tai `shelf` web-palvelimen toiminnallisuuksiin, vaikkakin suora tiedoston kirjoittaminen tyypillisesti nojaa sisäänrakennettuihin Dart-kirjastoihin.

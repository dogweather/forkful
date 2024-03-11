---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:48.103900-07:00
description: "Satunnaislukujen generointi Dart-kieless\xE4 sis\xE4lt\xE4\xE4 arvaamattomien\
  \ numeeristen arvojen luomisen, jotka eroavat jokaisella suorituskerralla. Ohjelmoijat\u2026"
lastmod: '2024-03-11T00:14:30.197917-06:00'
model: gpt-4-0125-preview
summary: "Satunnaislukujen generointi Dart-kieless\xE4 sis\xE4lt\xE4\xE4 arvaamattomien\
  \ numeeristen arvojen luomisen, jotka eroavat jokaisella suorituskerralla. Ohjelmoijat\u2026"
title: Satunnaisten numeroiden luominen
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen generointi Dart-kielessä sisältää arvaamattomien numeeristen arvojen luomisen, jotka eroavat jokaisella suorituskerralla. Ohjelmoijat hyödyntävät tätä toiminnallisuutta monista syistä, simuloidakseen todellisia tilanteita testiympäristöissä, mahdollistaakseen pelimekaniikkoja ja varmistaakseen turvallisuuden satunnaisuuden kautta kryptografisissa toiminnoissa.

## Kuinka:

Dartin ydin kirjasto tukee satunnaislukujen generointia `Random` luokan avulla, joka löytyy `dart:math`-kirjastosta. Tässä yksinkertainen esimerkki:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Generoi satunnaisen kokonaisluvun välillä 0 ja 99
  double randomDouble = rand.nextDouble(); // Generoi satunnaisen liukuluvun välillä 0.0 ja 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Esimerkkituloste: (Tämä vaihtelee joka kerta kun sitä ajetaan)*

```
23
0.6722390975465775
```

Käyttötapauksissa, jotka vaativat kryptografista satunnaisuutta, Dart tarjoaa `Random.secure` rakentajan:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Esimerkkituloste: (Tämä vaihtelee joka kerta kun sitä ajetaan)*

```
45
```

Jos työskentelet Flutter-projektien parissa tai tarvitset monimutkaisempaa satunnaisuutta, saatat löytää `faker` paketin hyödyllisenä, kun haluat generoida laajan valikoiman satunnaisia tietoja, kuten nimiä, osoitteita ja päivämääriä.

`faker`in käyttämiseksi, lisää se ensin `pubspec.yaml` tiedostoosi:

```yaml
dependencies:
  faker: ^2.0.0
```

Sen jälkeen, tuo se sisään ja käytä näin:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Generoi satunnaisen nimen
  print(faker.address.city()); // Generoi satunnaisen kaupungin nimen
}
```

*Esimerkkituloste:*

```
Josie Runolfsdottir
East Lysanne
```

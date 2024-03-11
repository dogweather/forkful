---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:42.248286-07:00
description: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen\
  \ on yleinen teht\xE4v\xE4 ohjelmoijille, jotka ty\xF6skentelev\xE4t aikataulutusten,\
  \ muistutusten tai mink\xE4\u2026"
lastmod: '2024-03-11T00:14:30.217517-06:00'
model: gpt-4-0125-preview
summary: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen on\
  \ yleinen teht\xE4v\xE4 ohjelmoijille, jotka ty\xF6skentelev\xE4t aikataulutusten,\
  \ muistutusten tai mink\xE4\u2026"
title: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tulevaisuuden tai menneisyyden päivämäärän laskeminen on yleinen tehtävä ohjelmoijille, jotka työskentelevät aikataulutusten, muistutusten tai minkä tahansa päivämäärälaskuihin perustuvan ominaisuuden parissa. Päivämäärien käsittelyn ymmärtäminen on ratkaisevan tärkeää taustajärjestelmille, käyttöliittymille ja tiedon analysoinnille, erityisesti niille, jotka siirtyvät käyttämään Dartia ja haluavat toteuttaa ajallista logiikkaa tehokkaasti.

## Miten:
Dart tarjoaa vankkaa tukea päivämäärämanipulaatiolle `DateTime`-luokkansa kautta. Tässä on, miten voit laskea tulevaisuuden tai menneisyyden päivämääriä käyttäen natiivia Dartia, ilman kolmansien osapuolien kirjastoja.

### Tulevaisuuden päivämäärän laskeminen
Tulevaisuuden päivämäärän laskemiseksi luot `DateTime`-objektin ja käytät `add`-metodia halutulla kestolla.

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // Tuloste: 2023-04-21 14:22:35.123456 (esimerkkituloste, riippuu nykyisestä päivämäärästä ja ajasta)
```

### Menneisyyden päivämäärän laskeminen
Menneisyyden päivämäärän laskemiseksi käytät `subtract`-metodia `DateTime`-objektille tarvittavalla kestolla.

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // Tuloste: 2023-03-27 14:22:35.123456 (esimerkkituloste, riippuu nykyisestä päivämäärästä ja ajasta)
```

### Kolmansien osapuolien kirjastojen käyttäminen
Vaikka Dartin natiivit valmiudet päivämäärämanipulaatioon ovatkin tehokkaita, saatat tarvita tarkempia toimintoja, kuten päivämäärien jäsennystä tai muotoilua helpommin, tai monimutkaisempia laskelmia. Tällaisissa tapauksissa `time`-paketti voi olla erittäin hyödyllinen.

Lisää ensin `time` riippuvuuksiisi `pubspec.yaml`-tiedostossa:

```yaml
dependencies:
  time: ^2.0.0
```

Sen jälkeen voit käyttää sitä suorittamaan samankaltaisia laskelmia parannetulla luettavuudella:

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // Tulevaisuuden päivämäärän laskeminen
  DateTime futureDate = today + 10.days;
  print(futureDate); // Tulostemuoto: 2023-04-21 14:22:35.123456

  // Menneisyyden päivämäärän laskeminen
  DateTime pastDate = today - 15.days;
  print(pastDate); // Tulostemuoto: 2023-03-27 14:22:35.123456
}
```

Nämä esimerkit havainnollistavat perus päivämäärämanipulaatioita Dartissa, mukaan lukien ajan lisääminen ja vähentäminen nykyisestä päivämäärästä, osoittaen kuinka vaivattomasti päivämääriä voidaan hallita Dart-sovelluksissa.

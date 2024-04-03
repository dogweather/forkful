---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:59.628983-07:00
description: "Kahden p\xE4iv\xE4m\xE4\xE4r\xE4n vertaaminen Dartissa sis\xE4lt\xE4\
  \xE4 niiden ajallisen eron tai j\xE4rjestyksen arvioimisen, mik\xE4 on olennainen\
  \ toiminnallisuus sovelluksissa,\u2026"
lastmod: '2024-03-13T22:44:56.284563-06:00'
model: gpt-4-0125-preview
summary: "Kahden p\xE4iv\xE4m\xE4\xE4r\xE4n vertaaminen Dartissa sis\xE4lt\xE4\xE4\
  \ niiden ajallisen eron tai j\xE4rjestyksen arvioimisen, mik\xE4 on olennainen toiminnallisuus\
  \ sovelluksissa, jotka hallinnoivat tapahtumia, m\xE4\xE4r\xE4aikoja tai mit\xE4\
  \ tahansa aikaan sidottua dataa."
title: "Kahden p\xE4iv\xE4m\xE4\xE4r\xE4n vertaaminen"
weight: 27
---

## Miten:
Dartissa voit verrata päivämääriä käyttämällä `DateTime`-luokkaa, joka tarjoaa suoria vertailumetodeja kuten `isBefore`, `isAfter` ja `isAtSameMomentAs`. Lisäksi päivämäärien välisen eron voi määrittää käyttämällä `difference()`-metodia, joka antaa `Duration`-objektin, joka yksityiskohtaisesti kertoo ajanjakson kahden aikapisteen välillä.

Tässä on yksinkertainen esimerkki näiden käsitteiden havainnollistamiseksi:

```dart
void main() {
  DateTime eventStart = DateTime(2023, 5, 15);
  DateTime eventEnd = DateTime(2023, 5, 20);
  
  // Tarkistetaan, onko yksi päivämäärä ennen toista
  if (eventStart.isBefore(eventEnd)) {
    print("Tapahtuman alkamispäivämäärä on ennen tapahtuman päättymispäivämäärää.");
  }

  // Tarkistetaan, ovatko kaksi päivämäärää samat
  if (!eventStart.isAtSameMomentAs(eventEnd)) {
    print("Aloituspäivämäärä ja päättymispäivämäärä eivät ole samat.");
  }
  
  // Lasketaan kahden päivämäärän välinen ero
  Duration eventDuration = eventEnd.difference(eventStart);
  print("Tapahtuma kestää ${eventDuration.inDays} päivää.");
}

/*
Tuloste:
Tapahtuman alkamispäivämäärä on ennen tapahtuman päättymispäivämäärää.
Aloituspäivämäärä ja päättymispäivämäärä eivät ole samat.
Tapahtuma kestää 5 päivää.
*/
```

Edistyneempiin päivämäärämanipulaatioihin, kuten muotoilumuunnoksiin, saatat löytää `DateFormat`-luokan `intl`-paketista hyödyllisenä. Alla on esimerkki, joka näyttää, kuinka sitä käytetään päivämäärien muotoiluun ja vertailemiseen:

Ensiksi, sisällytä `intl`-paketti `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  intl: ^0.17.0
```

Sitten käytä sitä seuraavasti:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime departureDate = DateTime(2023, 5, 15);
  DateTime returnDate = DateTime.parse('2023-05-20');

  // Päivämäärien muotoilu
  var formatter = DateFormat('yyyy-MM-dd');
  print("Lähtö: ${formatter.format(departureDate)}");
  print("Paluu: ${formatter.format(returnDate)}");

  // Vertaa käyttäen muotoiltuja merkkijonoja
  if (formatter.format(departureDate) == formatter.format(returnDate)) {
    print("Lähtö- ja paluupäivämäärät ovat samat.");
  } else {
    print("Lähtö- ja paluupäivämäärät ovat erilaiset.");
  }
}

/*
Tuloste:
Lähtö: 2023-05-15
Paluu: 2023-05-20
Lähtö- ja paluupäivämäärät ovat erilaiset.
*/
```

Tämä esimerkki esittelee, kuinka vertailla kahta `DateTime`-objektia sekä suoraan että käyttämällä muotoiltuja merkkijonoja vertailuissa, joissa tarvitsee jättää huomiotta tiettyjä komponentteja, kuten aika.

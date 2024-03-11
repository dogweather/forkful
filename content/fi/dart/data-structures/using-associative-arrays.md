---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:58.866524-07:00
description: "Associatiiviset taulukot Dartissa, tunnetaan yleisesti nimell\xE4 Mapit,\
  \ ovat tietorakenteita, jotka tallentavat tietoja avain-arvo -pareina. Ne\u2026"
lastmod: '2024-03-11T00:14:30.194303-06:00'
model: gpt-4-0125-preview
summary: "Associatiiviset taulukot Dartissa, tunnetaan yleisesti nimell\xE4 Mapit,\
  \ ovat tietorakenteita, jotka tallentavat tietoja avain-arvo -pareina. Ne\u2026"
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Associatiiviset taulukot Dartissa, tunnetaan yleisesti nimellä Mapit, ovat tietorakenteita, jotka tallentavat tietoja avain-arvo -pareina. Ne mahdollistavat ohjelmoijille elementtien käytön indeksien sijaan avaimilla, mikä tekee datan hausta intuitiivista ja tehokasta, erityisesti kun työskennellään rakenteellisen datan kanssa, jossa jokaisella elementillä on uniikki tunniste.

## Miten:

Dart tarjoaa suoraviivaisen syntaksin Map-tietorakenteiden luomiseen ja käsittelyyn. Alla on esimerkkejä perustoiminnoista, kuten luomisesta, elementtien lisäämisestä ja arvojen hakemisesta.

```dart
void main() {
  // Kartan luominen
  var hedelmanVärit = {
    'omena': 'punainen',
    'banaani': 'keltainen',
    'viinirypäle': 'violetti'
  };

  // Uuden avain-arvo -parin lisääminen
  hedelmanVärit['appelsiini'] = 'oranssi';

  // Arvon hakeminen avaimella
  print(hedelmanVärit['omena']); // Tuloste: punainen

  // Arvon päivittäminen
  hedelmanVärit['banaani'] = 'vihreä';

  // Iterointi Mapin yli
  hedelmanVärit.forEach((hedelma, vari) {
    print('$hedelma: $vari');
  });
  // Esimerkkituloste:
  // omena: punainen
  // banaani: vihreä
  // viinirypäle: violetti
  // appelsiini: oranssi
}
```

Monimutkaisten tietorakenteiden tai laajennetun toiminnallisuuden tapauksessa Dart ohjelmoijat turvautuvat usein lisäkirjastoihin. Yksi tällainen kirjasto on `collection`, joka tarjoaa edistyneitä kokoelmatyyppejä ja apuvälineitä. Vaikka `collection` ei muuta Map-tietorakenteiden peruskäyttöä, se rikastuttaa niitä hyödyllisillä funktioilla ja monimutkaisemmilla kokoelmatyypeillä. Tässä on, miten voit käyttää sitä tietyn tehtävän suorittamiseen, kuten Mapin järjestämiseen sen arvojen mukaan:

Varmista ensin, että `collection` -paketti sisältyy `pubspec.yaml` -tiedostoosi:

```yaml
dependencies:
  collection: ^1.15.0
```

Sen jälkeen voit käyttää sitä seuraavasti:

```dart
import 'package:collection/collection.dart';

void main() {
  var hedelmanVärit = {
    'omena': 'punainen',
    'banaani': 'keltainen',
    'viinirypäle': 'violetti',
    'appelsiini': 'oranssi'
  };

  // Järjestää Mapin sen arvojen (värien) mukaan
  var jarjestetytHedelmatVarinMukaan = SplayTreeMap.from(
    hedelmanVärit,
    (avain1, avain2) => hedelmanVärit[avain1]!.compareTo(hedelmanVärit[avain2]!)
  );

  print(jarjestetytHedelmatVarinMukaan);
  // Tuloste:
  // {appelsiini: oranssi, omena: punainen, banaani: keltainen, viinirypäle: violetti}
}
```

Tämä esimerkki demonstroi Map-tietueiden järjestämistä niiden arvojen perusteella, ja se näyttää, miten Dart ja sen elinvoimainen ekosysteemi voivat näppärästi käsitellä associatiivisia taulukoita monimutkaisempaan datan käsittelyyn.

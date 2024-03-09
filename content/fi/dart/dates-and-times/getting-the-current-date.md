---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-03-08T21:54:39.083043-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Nykyisen päivämäärän saaminen Dartissa sisältää järjestelmän kyselyn nykyisen päivämäärän ja ajan osalta. Tätä toiminnallisuutta käytetään yleisesti sovelluksissa ominaisuuksiin, kuten tapahtumien aikaleimaus, nykyisen päivämäärän näyttäminen käyttäjille tai kestojen laskeminen. Nykyisen päivämäärän tehokas noutaminen ja käsittely on perustavanlaatuinen aikataulutusta, lokiintia ja aikaan liittyviä ominaisuuksia varten.

## Miten:
Dartin ydin kirjasto tarjoaa suoraviivaisen pääsyn nykyiseen päivämäärään ja aikaan `DateTime` luokan kautta. Tässä on perusesimerkki nykyisen päivämäärän saamiseksi:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Esimerkkituloste: 2023-04-12 10:00:00.000
}
```

Jos tarvitset vain päivämääräosan (vuosi, kuukausi, päivä), voit muotoilla `DateTime` objektin:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Esimerkkituloste: 2023-04-12
}
```

Dart ei sisällä sisäänrakennettua kirjastoa monimutkaisemmalle päivämäärän muotoilulle, mutta tähän tarkoitukseen voi käyttää `intl` pakettia. Lisää ensin paketti `pubspec.yaml` tiedostoosi:

```yaml
dependencies:
  intl: ^0.17.0
```

Sitten, voit muotoilla päivämäärät helposti:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Esimerkkituloste: 2023-04-12
}
```

Tutustu `DateFormat` luokkaan, jonka `intl` paketti tarjoaa, saadaksesi lisää edistyneitä muotoiluvaihtoehtoja. Se tukee laajaa valikoimaa malleja ja kielialueita.

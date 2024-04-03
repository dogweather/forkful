---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:21.609348-07:00
description: 'Kuinka: #.'
lastmod: '2024-03-13T22:44:56.280052-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Koodin uudelleenj\xE4rjest\xE4minen"
weight: 19
---

## Kuinka:


### Esimerkki 1: Uudelleennimeäminen ja metodeiden erottaminen
Ennen refaktorointia saatat omistaa koodin, joka sekoittaa eri abstraktiotasoja tai vastuita, kuten alennuksen laskeminen ja sen soveltaminen:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Lopullinen hinta: $finalPrice");
}
```

**Tuloste:**
```
Lopullinen hinta: 80.0
```

Refaktoroinnin jälkeen voit erottaa alennuksen laskemisen omaan metodiinsa ja antaa sille merkityksellisen nimen:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = laskeLoppuhinta(price, discount);
  print("Lopullinen hinta: $finalPrice");
}

double laskeLoppuhinta(double price, double discount) {
  return price - (price * discount);
}
```

**Tuloste:**
```
Lopullinen hinta: 80.0
```

Laskennan erottamalla metodiin, sinulla on nyt selkeästi määritelty toiminto, jota voidaan käyttää uudelleen, testata itsenäisesti ja muokata helposti.

### Esimerkki 2: Ehtolausekkeiden yksinkertaistaminen
Ennen refaktorointia, ehtolauseet saattavat olla liian monimutkaisia tai vaikeita lukea:

```dart
void main() {
  var asiakastyyppi = "vakio";
  double alennus;
  
  if (asiakastyyppi == "vakio") {
    alennus = 0.05;
  } else if (asiakastyyppi == "jäsen") {
    alennus = 0.1;
  } else {
    alennus = 0.0;
  }

  print("Alennus: $alennus");
}
```

**Tuloste:**
```
Alennus: 0.05
```

Refaktoroinnin jälkeen harkitse käyttäväsi karttaa (map) selkeämmän rakenteen saavuttamiseksi ja asiakastyyppeihin ja alennuksiin liittyvien päivitysten tai laajennusten helppouttamiseksi:

```dart
void main() {
  var asiakastyyppi = "vakio";
  var alennukset = {
    "vakio": 0.05,
    "jäsen": 0.1,
    "ei": 0.0,
  };

  var alennus = alennukset[asiakastyyppi] ?? 0.0;
  print("Alennus: $alennus");
}
```

**Tuloste:**
```
Alennus: 0.05
```

Tämä uudelleenjärjestely ei ainoastaan tee koodista tiiviimpää, vaan myös kapseloi alennusten määrittämisen logiikan tavalla, joka on helpompi ymmärtää ja ylläpitää.

### Kolmannen osapuolen kirjastot refaktorointiin
Dartissa refaktoroidessa, erityisesti Flutter-sovelluksissa, [Dart DevTools](https://dart.dev/tools/dart-devtools) -työkalusarja on korvaamaton. Se sisältää suorituskykytyökaluja, widget-tarkastajan ja lähdekooditason virheenjäljittimen. Vaikka Dart DevTools ei olekaan kolmannen osapuolen kirjasto, sitä käytetään usein yhdessä esimerkiksi `flutter_bloc`-kirjaston kanssa puhtaan tilanhallinnan aikaansaamiseksi tavalla, joka edistää modulaarisuuden ja luettavuuden parantamista refactoroinnissa. Valitettavasti tämän merkinnän laajuuden vuoksi tiettyjä koodiesimerkkejä käyttäen kolmannen osapuolen kirjastoja ei tarjota tässä, mutta kehittäjiä kannustetaan tutkimaan näitä työkaluja parantaakseen refaktorointiprosessiaan Dart/Flutter-sovelluksissaan.

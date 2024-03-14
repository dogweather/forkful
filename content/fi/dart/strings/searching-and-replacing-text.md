---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:18.598417-07:00
description: "Tekstin etsiminen ja korvaaminen Dartissa k\xE4sitt\xE4\xE4 merkkijonojen\
  \ tutkiskelua tietyt mallit tai merkkijonojen sekvenssit l\xF6yt\xE4\xE4kseen ja\
  \ korvatakseen ne\u2026"
lastmod: '2024-03-13T22:44:56.254290-06:00'
model: gpt-4-0125-preview
summary: "Tekstin etsiminen ja korvaaminen Dartissa k\xE4sitt\xE4\xE4 merkkijonojen\
  \ tutkiskelua tietyt mallit tai merkkijonojen sekvenssit l\xF6yt\xE4\xE4kseen ja\
  \ korvatakseen ne\u2026"
title: Tekstin etsiminen ja korvaaminen
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin etsiminen ja korvaaminen Dartissa käsittää merkkijonojen tutkiskelua tietyt mallit tai merkkijonojen sekvenssit löytääkseen ja korvatakseen ne uudella sisällöllä. Tämä toimenpide on perustavanlaatuinen tehtäville, kuten datan validointi, tulosteen muotoilu, käyttäjän syötteen jäsennys tai jopa URL-osoitteiden ja tiedostopolkujen manipulointi, tehden sovelluksista dynaamisempia ja vastaamaan paremmin käyttäjän tarpeita.

## Kuinka:

Dart tarjoaa vankkoja menetelmiä tekstien etsimiseen ja korvaamiseen suoraan sen `String`-luokan kautta, ilman ulkoisten kirjastojen tarvetta. Näin voit tehdä sen:

### Perusetsintä ja -korvaus

Voit etsiä alimerkkijonoa ja korvata sen toisella merkkijonolla käyttämällä `replaceAll`-metodia:

```dart
String sampleText = "Hello, Dart! Dart on mahtava.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Tuloste: Hello, Flutter! Flutter on mahtava.
```

### Säännöllisten lausekkeiden käyttö

Monimutkaisempien etsintä- ja korvaustarpeiden osalta Dart käyttää säännöllisiä lausekkeita `RegExp`-luokan kautta. Tämä mahdollistaa mallien vastaavuuden etsimisen ja korvaamisen merkkijonoissa:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Tuloste: Dart 2024, Flutter 2024
```

Tässä esimerkissä etsitään kaikki merkkijonon yksi tai useampi numero (`\d+`) ja korvataan ne "2024":llä.

### Kirjainkoosta riippumaton etsintä

Suorittaaksesi kirjainkoosta riippumattoman etsinnän, voit muuttaa `RegExp`-konstruktorin ohittamaan kirjainkoon:

```dart
String sampleText = "Tervetuloa Dart, ohjelmointikieli.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Tuloste: Tervetuloa Flutter, ohjelmointikieli.
```

### Korvaaminen funktion avulla

Dynamisille korvauksille, jotka perustuvat itse osumaan, Dart mahdollistaa funktion välittämisen `replaceAllMapped`-metodille. Tämä funktio voi suorittaa operaatioita tai laskelmia vastaavien sekvenssien kanssa:

```dart
String sampleText = "Lisää 5:ttä yhdellä saadaksesi 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Tuloste: Lisää 6:ta yhdellä saadaksesi 7.
```

Tämä korvaa jokaisen numerosekvenssin sen inkrementtiarvolla. Jokainen osuma jäsentyy kokonaisluvuksi, inkrementoituu ja muunnetaan sitten takaisin merkkijonoksi korvaamista varten.

Dartin merkkijonojen käsittelykyvyt, erityisesti tekstien etsiminen ja korvaaminen, tekevät siitä tehokkaan työkalun datan käsittelyyn ja valmisteluun sovelluksissasi. Olitpa käyttämässä suoraviivaisia merkkijonokorvauksia tai hyödyntämässä säännöllisten lausekkeiden voimaa, Dart tarjoaa joustavuuden ja suorituskyvyn, jotka ovat tarpeen tehokkaaseen tekstikäsittelyyn.

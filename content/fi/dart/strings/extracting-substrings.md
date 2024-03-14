---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:06.067553-07:00
description: "Merkkijonojen osien erottelu liittyy tiettyjen merkkijonojen osien noutamiseen\
  \ niiden sijainnin tai mallien perusteella. Ohjelmoijat tekev\xE4t t\xE4m\xE4n\u2026"
lastmod: '2024-03-13T22:44:56.258628-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonojen osien erottelu liittyy tiettyjen merkkijonojen osien noutamiseen\
  \ niiden sijainnin tai mallien perusteella. Ohjelmoijat tekev\xE4t t\xE4m\xE4n\u2026"
title: Alimerkkijonojen erottaminen
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonojen osien erottelu liittyy tiettyjen merkkijonojen osien noutamiseen niiden sijainnin tai mallien perusteella. Ohjelmoijat tekevät tämän tehtäviä varten kuten käyttäjäsyötteen jäsentäminen, datan käsittely tai oleellisen tiedon erottaminen suuremmista tekstilähteistä.

## Kuinka:
Dartissa voit käyttää erilaisia menetelmiä merkkijonojen osien erotteluun, kuten `substring()`, `split()` ja säännölliset lausekkeet. Jokainen menetelmä palvelee eri tarkoituksia ja tarjoaa joustavuutta merkkijonojen käsittelyssä.

### Käyttäen `substring()`:
`substring()`-metodi on suoraviivainen. Määrität aloitusindeksin (ja valinnaisesti, loppuindeksin) merkkijonon leikkaukseen.

```dart
void main() {
  String example = "Hei, maailma!";
  String result = example.substring(7, 12);
  print(result); // Tuloste: maail
}
```

### Käyttäen `split()`:
Jaa merkkijono listaksi osamerkkijonoja perustuen malliin (kuten välilyönti tai pilkku) ja sen jälkeen pääset käsiksi osamerkkijonoon indeksin perusteella.

```dart
void main() {
  String example = "Dart on hauskaa";
  List<String> parts = example.split(' ');
  String result = parts[1]; // Pääsy indeksillä
  print(result); // Tuloste: on
}
```

### Käyttäen säännöllisiä lausekkeita:
Monimutkaisiin malleihin Dartin `RegExp`-luokka on tehokas. Käytä sitä mallien vastaavuuden tarkistamiseen ja osamerkkijonojen erotteluun.

```dart
void main() {
  String example = "Sähköposti: esimerkki@meili.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // Tuloste: esimerkki@meili.com
}
```

### Kolmannen osapuolen kirjastot:
Vaikka Dartin vakiokirjasto onkin melko pätevä, saatat kohdata skenaarioita, joissa kolmannen osapuolen kirjasto voisi yksinkertaistaa tehtävääsi. Suosittu valinta merkkijonojen käsittelyyn ja mallien vastaavuuden tarkistamiseen ei ole erityisesti suositeltu tässä, koska Dartin sisäiset ominaisuudet usein riittävät. Tarkista kuitenkin aina [pub.dev](https://pub.dev) mahdollisista kirjastoista, jotka saattavat paremmin sopia erityistarpeisiisi.

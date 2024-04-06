---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:33.253982-07:00
description: "Kuinka: 1. **Asenna Dart**: Varmista, ett\xE4 Dart on asennettu j\xE4\
  rjestelm\xE4\xE4si. Jos ei, voit ladata sen osoitteesta [https://dart.dev/get-\u2026"
lastmod: '2024-04-05T22:38:56.870967-06:00'
model: gpt-4-0125-preview
summary: "1. **Asenna Dart**: Varmista, ett\xE4 Dart on asennettu j\xE4rjestelm\xE4\
  \xE4si. Jos ei, voit ladata sen osoitteesta [https://dart.dev/get-dart](https://dart.dev/get-dart).\
  \ Vahvista asennus komennolla."
title: Uuden projektin aloittaminen
weight: 1
---

## Kuinka:
1. **Asenna Dart**:
   Varmista, että Dart on asennettu järjestelmääsi. Jos ei, voit ladata sen osoitteesta [https://dart.dev/get-dart](https://dart.dev/get-dart). Vahvista asennus komennolla:

   ```shell
   dart --version
   ```

2. **Luo Uusi Dart-projekti**:
   Käytä Dart CLI:tä uuden projektin luomiseen:

   ```shell
   dart create hello_dart
   ```

   Tämä komento luo uuden hakemiston `hello_dart` yksinkertaisen esimerkkiweb- tai konsolisovelluksen kanssa, riippuen valinnastasi.

3. **Tutki Projektin Rakenne**:
   
   Siirry projektihakemistoosi:

   ```shell
   cd hello_dart
   ```

   Tyypillinen Dart-projekti sisältää seuraavat keskeiset tiedostot ja hakemistot:

   - `pubspec.yaml`: Asetustiedosto, joka sisältää projektisi riippuvuudet ja SDK-rajoitukset.
   - `lib/`: Hakemisto, jossa suurin osa Dart-koodista sijaitsee.
   - `test/`: Hakemisto projektin testeille.

4. **Lisää Riippuvuuksia**:
   Muokkaa `pubspec.yaml`-tiedostoa lisätäksesi riippuvuuksia. Web-projekteille harkitse `http`:n lisäämistä, joka on suosittu paketti HTTP-pyyntöjen tekemiseen:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Muokkauksen jälkeen hanki riippuvuudet:

   ```shell
   dart pub get
   ```

5. **Kirjoita Ensimmäinen Dart-koodisi**:
   
   Luo `lib/`-hakemistoon uusi Dart-tiedosto, `main.dart`, ja lisää yksinkertainen Dart-koodi:

   ```dart
   // Tuo Dart-ydin kirjasto
   import 'dart:core';

   void main() {
     print('Hei, Dart!');
   }
   ```

6. **Suorita Dart-sovelluksesi**:

   Suorita Dart-ohjelmasi komennolla:

   ```shell
   dart run
   ```

   Tuloksen pitäisi olla:

   ```
   Hei, Dart!
   ```

Näitä vaiheita seuraamalla olet onnistuneesti aloittanut uuden Dart-projektin asennuksesta ensimmäisen Dart-koodikappaleen suorittamiseen. Tämä perustieto luo pohjan sukeltamiselle syvemmälle Dartin rikkaaseen ekosysteemiin ja sen kykyihin rakentaa skaalautuvia sovelluksia.

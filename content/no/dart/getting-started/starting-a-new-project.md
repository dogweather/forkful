---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:51.554502-07:00
description: "Hvordan: 1. **Installer Dart**: S\xF8rg for at Dart er installert p\xE5\
  \ systemet ditt. Hvis ikke, kan du laste det ned fra [https://dart.dev/get-\u2026"
lastmod: '2024-04-05T21:53:41.466784-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## Hvordan:
1. **Installer Dart**:
   Sørg for at Dart er installert på systemet ditt. Hvis ikke, kan du laste det ned fra [https://dart.dev/get-dart](https://dart.dev/get-dart). Verifiser installasjonen med:

   ```shell
   dart --version
   ```

2. **Opprett et Nytt Dart-prosjekt**:
   Bruk Dart CLI for å generere et nytt prosjekt:

   ```shell
   dart create hello_dart
   ```

   Denne kommandoen oppretter en ny mappe `hello_dart` med en enkel prøve web- eller konsollapplikasjon, avhengig av ditt valg.

3. **Undersøk Prosjektstrukturen**:

   Naviger til prosjektmappen din:

   ```shell
   cd hello_dart
   ```

   Et typisk Dart-prosjekt inkluderer følgende nøkkelfiler og mapper:

   - `pubspec.yaml`: Konfigurasjonsfil som inkluderer prosjektets avhengigheter og SDK-begrensninger.
   - `lib/`: Mappe hvor mesteparten av Dart-koden befinner seg.
   - `test/`: Mappe for prosjekttester.

4. **Legg til Avhengigheter**:
   Rediger `pubspec.yaml` for å legge til avhengigheter. For webprosjekter, vurder å legge til `http`, en populær pakke for å gjøre HTTP-forespørsler:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Etter redigering, få avhengighetene:

   ```shell
   dart pub get
   ```

5. **Skriv Ditt Første Dart-kode**:

   I `lib/`-mappen, opprett en ny Dart-fil, `main.dart`, og legg til enkel Dart-kode:

   ```dart
   // Importer Dart-kjernebiblioteket
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **Kjør Din Dart-applikasjon**:

   Utfør Dart-programmet ditt med:

   ```shell
   dart run
   ```

   Resultatet bør være:

   ```
   Hello, Dart!
   ```

Ved å følge disse stegene har du vellykket startet et nytt Dart-prosjekt, fra installasjon til å kjøre ditt første stykke Dart-kode. Denne grunnleggende kunnskapen setter scenen for å dykke dypere inn i Darts rike økosystem og dets evner til å bygge skalerbare applikasjoner.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:37.073589-07:00
description: "Hur man g\xF6r: 1. **Installera Dart**: Se till att Dart \xE4r installerat\
  \ p\xE5 ditt system. Om inte, kan du ladda ner det fr\xE5n [https://dart.dev/get-\u2026"
lastmod: '2024-04-05T22:37:46.297196-06:00'
model: gpt-4-0125-preview
summary: "1. **Installera Dart**: Se till att Dart \xE4r installerat p\xE5 ditt system.\
  \ Om inte, kan du ladda ner det fr\xE5n [https://dart.dev/get-dart](https://dart.dev/get-dart).\
  \ Verifiera installationen med."
title: Att starta ett nytt projekt
weight: 1
---

## Hur man gör:
1. **Installera Dart**:
   Se till att Dart är installerat på ditt system. Om inte, kan du ladda ner det från [https://dart.dev/get-dart](https://dart.dev/get-dart). Verifiera installationen med:

   ```shell
   dart --version
   ```

2. **Skapa ett nytt Dart-projekt**:
   Använd Dart CLI för att generera ett nytt projekt:

   ```shell
   dart create hello_dart
   ```

   Detta kommando skapar en ny mapp `hello_dart` med ett enkelt exempel på en webb- eller konsolapplikation, beroende på ditt val.

3. **Undersök projektstrukturen**:
   
   Navigera till din projektmapp:

   ```shell
   cd hello_dart
   ```

   Ett typiskt Dart-projekt inkluderar följande viktiga filer och kataloger:

   - `pubspec.yaml`: Konfigurationsfil som inkluderar projektets beroenden och SDK-begränsningar.
   - `lib/`: Katalog där största delen av Dart-koden finns.
   - `test/`: Katalog för projekttester.

4. **Lägg till beroenden**:
   Redigera `pubspec.yaml` för att lägga till beroenden. För webbprojekt, överväg att lägga till `http`, ett populärt paket för att göra HTTP-begäranden:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Efter redigering, hämta beroendena:

   ```shell
   dart pub get
   ```

5. **Skriv din första Dart-kod**:
   
   I `lib/` katalogen, skapa en ny Dart-fil, `main.dart`, och lägg till enkel Dart-kod:

   ```dart
   // Importera Dart-kärnbiblioteket
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **Kör ditt Dart-program**:

   Exekvera ditt Dart-program med:

   ```shell
   dart run
   ```

   Utmatningen bör vara:

   ```
   Hello, Dart!
   ```

Genom att följa dessa steg har du framgångsrikt startat ett nytt Dart-projekt, från installation till att köra din första bit av Dart-kod. Denna grundläggande kunskap lägger grunden för att dyka djupare in i Darts rika ekosystem och dess kapaciteter för att bygga skalbara applikationer.

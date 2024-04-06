---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:36.738915-07:00
description: "Jak to zrobi\u0107: 1. **Zainstaluj Dart**: Upewnij si\u0119, \u017C\
  e Dart jest zainstalowany na Twoim systemie. Je\u015Bli nie, mo\u017Cesz go pobra\u0107\
  \ z [https://dart.dev/get-\u2026"
lastmod: '2024-04-05T21:53:36.529500-06:00'
model: gpt-4-0125-preview
summary: ''
title: "Rozpocz\u0119cie nowego projektu"
weight: 1
---

## Jak to zrobić:
1. **Zainstaluj Dart**:
   Upewnij się, że Dart jest zainstalowany na Twoim systemie. Jeśli nie, możesz go pobrać z [https://dart.dev/get-dart](https://dart.dev/get-dart). Zweryfikuj instalację, używając:

   ```shell
   dart --version
   ```

2. **Utwórz Nowy Projekt Dart**:
   Użyj CLI Dart do wygenerowania nowego projektu:

   ```shell
   dart create hello_dart
   ```

   To polecenie tworzy nowy katalog `hello_dart` z prostą próbną aplikacją webową lub konsolową, w zależności od twojego wyboru.

3. **Zbadaj Strukturę Projektu**:
   
   Przejdź do katalogu swojego projektu:

   ```shell
   cd hello_dart
   ```

   Typowy projekt Dart zawiera następujące kluczowe pliki i katalogi:

   - `pubspec.yaml`: Plik konfiguracyjny, który zawiera zależności Twojego projektu i ograniczenia SDK.
   - `lib/`: Katalog, w którym znajduje się większość kodu Dart.
   - `test/`: Katalog do testów projektu.
   
4. **Dodaj Zależności**:
   Edytuj `pubspec.yaml`, aby dodać zależności. Dla projektów webowych, rozważ dodanie `http`, popularnego pakietu do wykonywania żądań HTTP:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Po edycji, pobierz zależności:

   ```shell
   dart pub get
   ```

5. **Napisz Swój Pierwszy Kod Dart**:
   
   W katalogu `lib/` utwórz nowy plik Dart, `main.dart`, i dodaj prosty kod Dart:

   ```dart
   // Importuj podstawową bibliotekę Dart
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **Uruchom Swoją Aplikację Dart**:

   Wykonaj swój program Dart za pomocą:

   ```shell
   dart run
   ```

   Wynik powinien być:

   ```
   Hello, Dart!
   ```

Podążając za tymi krokami, z sukcesem rozpocząłeś nowy projekt Dart, od instalacji do uruchomienia twojego pierwszego fragmentu kodu Dart. Ta podstawowa wiedza przygotowuje grunt pod głębsze zanurzenie w bogaty ekosystem Dart i jego możliwości tworzenia skalowalnych aplikacji.

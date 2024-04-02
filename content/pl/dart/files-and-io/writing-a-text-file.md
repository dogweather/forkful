---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:59.564994-07:00
description: "Zapisywanie pliku tekstowego w Dart polega na tworzeniu lub modyfikowaniu\
  \ plik\xF3w na dysku w celu przechowywania danych w formacie czytelnym. Programi\u015B\
  ci\u2026"
lastmod: '2024-03-13T22:44:35.112675-06:00'
model: gpt-4-0125-preview
summary: "Zapisywanie pliku tekstowego w Dart polega na tworzeniu lub modyfikowaniu\
  \ plik\xF3w na dysku w celu przechowywania danych w formacie czytelnym. Programi\u015B\
  ci\u2026"
title: Pisanie pliku tekstowego
weight: 24
---

## Co i dlaczego?
Zapisywanie pliku tekstowego w Dart polega na tworzeniu lub modyfikowaniu plików na dysku w celu przechowywania danych w formacie czytelnym. Programiści robią to, aby zapisać dane aplikacji, konfiguracje, logi lub jakiekolwiek informacje, które powinny pozostać między uruchomieniami aplikacji lub udostępnić dane innym aplikacjom lub użytkownikom.

## Jak to zrobić:
Biblioteka podstawowa Darta oferuje pakiet `dart:io` do obsługi plików, co pozwala na pisanie plików tekstowych bez potrzeby korzystania z bibliotek stron trzecich. Oto prosty przykład zapisywania pliku tekstowego:

```dart
import 'dart:io';

void main() async {
  // Utwórz nowy plik o nazwie 'example.txt' w bieżącym katalogu.
  var file = File('example.txt');
  
  // Zapisz ciąg znaków do pliku.
  await file.writeAsString('Hello, Dart!');
  
  // Zweryfikuj zawartość.
  print(await file.readAsString()); // Wyjście: Hello, Dart!
}
```

Przy pracy z większymi plikami lub strumieniami danych może być preferowane zapisywanie treści za pomocą `openWrite`, które zwraca `IOSink` i pozwala na zapisywanie danych partiami:

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // Zapisz wiele linii do pliku.
  sink
    ..writeln('Linia 1: Szybki brązowy lis przeskakuje przez leniwego psa.')
    ..writeln('Linia 2: Dart jest niesamowity!')
    ..close();

  // Poczekaj na zamknięcie zlewu, aby upewnić się, że wszystkie dane zostały zapisane do pliku.
  await sink.done;

  // Odczytaj i wydrukuj zawartość pliku, aby zweryfikować
  print(await file.readAsString());
}
```

Dla bardziej zaawansowanych operacji na plikach, w tym dołączania do plików lub zapisywania bajtów, możesz zagłębić się w metody klasy `File` dostarczone przez `dart:io`. Ponadto, pracując nad projektami większej skali lub bardziej złożonymi, warto rozważyć użycie pakietów takich jak `path` do obsługi ścieżek plików czy `shelf` do funkcji serwera internetowego, chociaż bezpośredni zapis plików zazwyczaj polega na wbudowanych bibliotekach Darta.

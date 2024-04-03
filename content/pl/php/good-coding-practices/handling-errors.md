---
date: 2024-01-26 00:56:33.033031-07:00
description: "Jak to zrobi\u0107: W PHP mo\u017Cesz zarz\u0105dza\u0107 b\u0142\u0119\
  dami za pomoc\u0105 blok\xF3w `try-catch`, a tak\u017Ce dostosowa\u0107 proces poprzez\
  \ w\u0142asne procedury obs\u0142ugi b\u0142\u0119d\xF3w i wyj\u0105tki."
lastmod: '2024-03-13T22:44:35.504900-06:00'
model: gpt-4-1106-preview
summary: "W PHP mo\u017Cesz zarz\u0105dza\u0107 b\u0142\u0119dami za pomoc\u0105 blok\xF3\
  w `try-catch`, a tak\u017Ce dostosowa\u0107 proces poprzez w\u0142asne procedury\
  \ obs\u0142ugi b\u0142\u0119d\xF3w i wyj\u0105tki."
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Jak to zrobić:
W PHP możesz zarządzać błędami za pomocą bloków `try-catch`, a także dostosować proces poprzez własne procedury obsługi błędów i wyjątki.

```php
// Podstawowy przykład try-catch
try {
  // Wykonaj coś ryzykownego
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // Obsłuż błąd
  echo "Błąd: " . $e->getMessage();
}

// Ustawienie własnej procedury obsługi błędów
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Używanie wyjątków
class MyException extends Exception {}

try {
  // Wykonaj coś i zgłoś własny wyjątek
  throw new MyException("Niestandardowy błąd!");
} catch (MyException $e) {
  // Obsłuż własny wyjątek
  echo $e->getMessage();
}

// Przykładowe wyjście:
// Błąd: fopen(nonexistentfile.txt): nie udało się otworzyć strumienia: Nie ma takiego pliku ani katalogu
// Niestandardowy błąd!
```

## Pogłębienie wiedzy
Kiedyś błędy w PHP były raczej ostrzeżeniami i komunikatami, które nie zatrzymywały wykonania skryptu. Wraz z dojrzewaniem języka, przyjął on bardziej zaawansowane obiektowe podejście do obsługi błędów za pomocą klasy Exception, wprowadzonej w PHP 5. Później, w PHP 7 pojawiły się klasy Error, które wreszcie różnicują między błędami a wyjątkami.

Przed blokami `try-catch`, PHP używał funkcji `set_error_handler()` do radzenia sobie z błędami. `try-catch` jest czystszym, nowocześniejszym rozwiązaniem. Ale własne procedury obsługi błędów nadal mają swoje miejsce, szczególnie w starszym kodzie lub gdy trzeba wyłapać to, co normalnie byłoby błędami niebędącymi wyjątkami.

Interfejs `Throwable` w PHP 7+ oznacza, że czy to błąd, czy wyjątek, obu możesz się łapać. To jest przydatne, ponieważ teraz nie przegapisz krytycznych błędów w czasie wykonania, które były wcześniej trudniejsze do śledzenia.

Alternatywy spoza wbudowanych mechanizmów PHP obejmują biblioteki i frameworki, które mają własne systemy obsługi błędów, oferując więcej funkcji, takich jak rejestrowanie błędów do plików czy wyświetlanie przyjaznych dla użytkownika stron błędów.

## Zobacz także
- Oficjalna dokumentacja PHP na temat wyjątków: https://www.php.net/manual/pl/language.exceptions.php
- PHP The Right Way na temat raportowania błędów: https://phptherightway.com/#error_reporting
- Podręcznik PHP na temat Obsługi Błędów: https://www.php.net/manual/pl/book.errorfunc.php

---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "PHP: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że jako programiści musimy operować na różnego rodzaju danych, w tym również na tekście. Czasami potrzebujemy zmienić tekst na małe litery, np. dla ujednolicenia danych lub wygodniejszego porównania. W tym artykule dowiesz się, jak w szybki i prosty sposób skonwertować string na małe litery w języku PHP.

## Jak to zrobić

```PHP
$string = "To Jest PRZyKłAd TeKstU";
echo strtolower($string);
```

Wynikiem powyższego kodu będzie tekst: "to jest przykład tekstu". W celu konwersji stringa na małe litery wykorzystujemy wbudowaną funkcję `strtolower()` w języku PHP. Funkcja ta zwraca przekazany do niej string w postaci z małymi literami.

Możemy także wykorzystać pętlę `for` w połączeniu z funkcją `strlen()` do iteracji po poszczególnych znakach w tekście i wywołania na nich funkcji `strtolower()`.

```PHP
$string = "String Do KOnWERSjI";
$stringLength = strlen($string);
for($i = 0; $i < $stringLength; $i++) {
    echo strtolower($string[$i]);
}
```

Wynikiem będzie ponownie tekst: "string do konwersji". Możemy także wykorzystać funkcję `mb_strtolower()` dla tekstów w kilku językach lub `strtoupper()` do konwersji na wielkie litery.

## Deep Dive

Konwersja stringa na małe lub wielkie litery może być również pomocna przy walidacji danych wprowadzonych przez użytkownika. Dzięki temu unikniemy problemów z różnicami w pisowni, co może prowadzić do błędów w systemie.

PHP udostępnia także funkcję `lcfirst()` do zamiany pierwszej litery w tekście na małą oraz `ucfirst()` do zamiany pierwszej litery na wielką.

Warto również pamiętać o ustawieniu domyślnego kodowania znaków w języku PHP za pomocą funkcji `mb_internal_encoding()`, aby uniknąć problemów z różnicami między systemami.

## Zobacz również

- [Dokumentacja PHP o `strtolower()`](https://www.php.net/manual/en/function.strtolower.php)
- [Przykłady użycia funkcji `strtolower()`](https://www.w3schools.com/php/func_string_strtolower.asp)
- [Inne funkcje do manipulacji tekstem w PHP](https://www.php.net/manual/en/ref.strings.php)
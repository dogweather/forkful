---
title:                "Pisanie do standardowego błędu"
html_title:           "PHP: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego wyjścia błędu ma wiele zastosowań w PHP. Najważniejszym powodem jest możliwość wykrywania i naprawiania błędów w kodzie. Dzięki temu można zoptymalizować działanie aplikacji i uniknąć nieprawidłowego zachowania.

## Jak to zrobić

Pisanie do standardowego wyjścia błędu w PHP jest bardzo proste. Wystarczy użyć funkcji `fwrite()` wraz z parametrem `STDERR`. Przykładowy kod wyglądałby następująco:

```PHP
fwrite(STDERR, "Błąd: Nie można otworzyć pliku!");
```

Gdy funkcja zostanie wykonana, tekst zostanie wyświetlony w konsoli lub zapisany do pliku dziennika błędów, w zależności od konfiguracji serwera.

Można również wykorzystać funkcję `error_log()` z parametrem `0` lub `4`, aby przesyłać błędy i ostrzeżenia do standardowego wyjścia błędu. Przykładowy kod wyglądałby tak:

```PHP
error_log("Błąd: Nie można podzielić przez zero!", 0);
```

Podczas wywoływania aplikacji z wiersza poleceń można również przekierować wyjście błędu do pliku używając operatora `2>`. Przykładowo:

```bash
php moja_aplikacja.php 2> errors.txt
```

## Głębszy wgląd

Pisanie do standardowego wyjścia błędu jest przydatne nie tylko do wykrywania błędów w kodzie, ale także do wyświetlania ostrzeżeń oraz informacji diagnostycznych. Można także wykorzystać funkcję `trigger_error()` wraz z odpowiednimi parametrami, aby dodatkowo zdefiniować typ błędu oraz wyświetlać stos śladów.

Pamiętaj, aby nie nadużywać funkcji piszących do standardowego wyjścia błędu, ponieważ mogą one znacząco wydłużyć czas wykonania kodu. Lepiej jest wykorzystać je tylko do naprawiania błędów w trakcie developmentu, a w produkcji skorzystać z innych metod raportowania błędów.

## Zobacz także

- [Dokumentacja PHP o funkcjach piszących do standardowego wyjścia błędu](https://www.php.net/manual/en/function.fwrite.php)
- [Artykuł na temat obsługi błędów w PHP](https://www.phptutorial.net/php-tutorial/php-error-handling/)
- [Poradnik dotyczący użycia funkcji `error_log()`](https://www.php.net/manual/en/function.error-log.php)
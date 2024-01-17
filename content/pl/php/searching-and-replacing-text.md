---
title:                "Wyszukiwanie i zamienianie tekstu"
html_title:           "PHP: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Szukanie i zamiana tekstu jest jedną z podstawowych czynności wykonywanych przez programistów podczas tworzenia oprogramowania. Polega ona na znajdowaniu określonych ciągów znaków w tekście i zastępowaniu ich innymi. Jest to często wykorzystywane w celu automatyzacji zadań i przyspieszenia procesu tworzenia kodu.

## Jak to zrobić:
Przykłady kodu poniżej wyjaśnią jak wykonać wyszukiwanie i zamianę tekstu za pomocą PHP. Można to zrobić używając funkcji `str_replace()` lub `preg_replace()`, a także wykorzystując wyrażenia regularne. Przykładowy kod oraz wynik działania prezentują się następująco:

```PHP
// Użycie funkcji str_replace()
$text = "Hej, to jest przykładowy tekst.";
$result = str_replace("przykładowy", "nowy", $text);
echo $result;

// Wynik: Hej, to jest nowy tekst.

// Użycie funkcji preg_replace()
$text = "Programowanie jest super!";
$result = preg_replace("/super/", "fajne", $text);
echo $result;

// Wynik: Programowanie jest fajne!
```

## Głębsze Pogłębienie:
Wyszukiwanie i zamiana tekstu jest popularną techniką wykorzystywaną w wielu językach programowania. W PHP występują różne funkcje i metody umożliwiające wykonanie tej czynności. Istnieją także alternatywne sposoby, takie jak wykorzystanie biblioteki `strtr()` lub wykorzystywanie wyrażeń regularnych bezpośrednio do przetwarzania danych z bazy danych. Co więcej, istnieje możliwość wykorzystania flag i opcji, które pozwalają na bardziej zaawansowane wyszukiwanie i zamianę tekstu.

## Zobacz także:
- Dokumentacja PHP dla funkcji `str_replace()`: https://www.php.net/manual/pl/function.str-replace.php
- Dokumentacja PHP dla funkcji `preg_replace()`: https://www.php.net/manual/pl/function.preg-replace.php
- Przykłady użycia wyrażeń regularnych w PHP: https://www.php.net/manual/pl/book.pcre.php
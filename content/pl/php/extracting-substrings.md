---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyodrębnianie podciągów to proces odzielenia specyficznej części ciągu znaków od reszty. Programiści robią to, aby manipulować lub analizować specyficzne segmenty danych tekstowych.

## Jak to zrobić:

Do wyodrębniania podciągów w PHP używamy na przykład funkcji `substr()`. Oto kilka przykładów użycia:

```PHP
<?php
$tekst = "Cześć, Świecie!";
echo substr($tekst, 0, 5);
?>
```

Powinno wyświetlić "Cześć".

```PHP
<?php
$tekst = "Cześć, Świecie!";
echo substr($tekst, 7, 8);
?>
```

Powinno wyświetlić "Świecie".

## Głębsze spojrzenie

Funkcja `substr()` była obecna w PHP od jego pierwszej stabilnej wersji (PHP 4). Wersja 5 funkcję rozszerzyła o obsługę znaków multibajtowych via `mb_substr()`. Choć `substr()` jest powszechnie używana, istnieją alternatywy, np. `strpos()` i `strrpos()`, które znajdują pozycję podciągu w ciągu.

Warto zwrócić uwagę na sposób, w jaki PHP obsługuje indeksy - PHP zaczyna numerować od zera. Dla wartości ujemnych, PHP zaczyna od końca ciągu. Domyślnie, jeśli drugi argument przekroczy długość ciągu, `substr()` zwróci cały ciąg od punktu startowego.

## Zobacz także:

1. Szczegółowy opis funkcji `substr()`: [PHP: substr - Manual](https://www.php.net/manual/en/function.substr.php)
2. Więcej na temat `strpos()` i `strrpos()`: [PHP: strpos - Manual](https://www.php.net/manual/en/function.strpos.php) oraz [PHP: strrpos - Manual](https://www.php.net/manual/en/function.strrpos.php)
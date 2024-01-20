---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Znajdowanie długości łańcucha to proces ustalania, ile znaków zawiera dany ciąg. Jest to niezbędne w wielu przypadkach: sortowanie, wyszukiwanie, walidacja danych, a także przy obróbce i manipulacji tekstem.

## Jak to zrobić:
Rozważmy przykładowe bloki kodu PHP ilustrujące, jak znaleźć długość łańcucha:
```PHP
$string = "Programowanie to zabawa";
echo strlen($string);
```
Wynikiem tego kodu będzie 23, co stanowi ilość znaków w ciągu "Programowanie to zabawa".

## Głębsze spojrzenie:
Początek funkcji strlen sięga jeszcze czasów języka C, gdzie była używana do ustalania długości łańcucha. Alternatywą dla strlen w PHP mogą być funkcje mb_strlen (dla łańcuchów multibajtowych) oraz grapheme_strlen (dla łańcuchów złożonych z wielu znaków graficznych).

Pamiętaj, że funkcja strlen działa na bajtach, a nie znakach. Oznacza to, że dla łańcuchów multibajtowych (np. UTF-8) wynik może nie być taki, jakiego oczekujesz. W takim przypadku użyj funkcji mb_strlen.

## Zobacz także:
1. Dokumentacja funkcji strlen w PHP: http://php.net/manual/pl/function.strlen.php
2. Artykuł na temat obsługi łańcuchów multibajtowych w PHP: https://www.php.net/manual/pl/book.mbstring.php
3. Szczegółowy przegląd różnych metod do liczenia długości łańcucha: https://stackoverflow.com/questions/2510434/how-to-find-out-the-number-of-characters-in-a-string
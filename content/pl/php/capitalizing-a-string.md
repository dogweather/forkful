---
title:                "Zamiana małych liter na wielkie w ciągu znaków"
html_title:           "PHP: Zamiana małych liter na wielkie w ciągu znaków"
simple_title:         "Zamiana małych liter na wielkie w ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zbiór znaków, nazywany stringiem, można przekształcić tak, aby każde słowo zaczynało się wielką literą. Robi się to w PHP, żeby podkreślić ważne informacje, ulepszyć czytelność lub spełnić specyficzne wymagania formatowania.

## Jak to zrobić:

W PHP użyjemy funkcji `ucwords()`, która konwertuje pierwszą literę każdego słowa w stringu na wielką literę. Przyjrzyjmy się temu:

```PHP
$str = 'witaj, świecie';
$capitalizedStr = ucwords($str);
echo $capitalizedStr;
```

Powyższy kod wypisze: 'Witaj, Świecie'

## Deep Dive:

Funkcja `ucwords()` pojawiła się w PHP od początku, czyli od wersji 4.0. Każda nowa wersja ją doskonaliła.

Alternatywą dla `ucwords()` jest funkcja `mb_convert_case()`, która jest lepszym rozwiązaniem, jeśli pracujesz z multibyte stringami.

Co do szczegółów implementacji, `ucwords()` w PHP używa reguł podobnych do 'C'. To znaczy, uważa za początek słowa każdą pozycję, która nie jest poprzedzona literą.

## Zobacz też:

- Dokumentacja PHP ucwords(): https://www.php.net/manual/pl/function.ucwords.php
- Dokumentacja PHP mb_convert_case(): https://www.php.net/manual/pl/function.mb-convert-case.php
- Szczegółowe omówienie różnic między wielkością liter: https://www.geeksforgeeks.org/php-ucwords-function/
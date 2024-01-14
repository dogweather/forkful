---
title:                "PHP: Łączenie łańcuchów znaków"
simple_title:         "Łączenie łańcuchów znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wykorzystanie konkatenacji ciągów znaków jest nieodzowne w PHP, szczególnie gdy chcemy dynamicznie tworzyć teksty lub generować wyjątki i komunikaty dla użytkownika. Jest to prosta, ale niezwykle przydatna metoda w programowaniu.

## Jak to zrobić

Konkatenacja w PHP polega na łączeniu dwóch lub więcej ciągów znaków w jeden. Można to zrobić za pomocą operatora kropki (.), który łączy dwa ciągi znaków, lub za pomocą funkcji `concat()`. Oto przykład wykorzystujący operator kropki:

```PHP
<?php
$string1 = "Hej";
$string2 = "Polacy";
echo $string1 . " " . $string2;
```

Output: `Hej Polacy`

Natomiast przykład z użyciem funkcji `concat()` wyglądałby tak:

```PHP
<?php
$string1 = "Witaj";
$string2 = "świecie";
echo concat($string1, $string2);
```

Output: `Witajświecie`

## Głębszy wgląd

Ważne jest, aby wiedzieć, jak przebiega konkatenacja wewnątrz silnika PHP. Za każdym razem, gdy następuje konkatenacja, tworzony jest nowy ciąg znaków w pamięci o rozmiarze sumy długości konkatenowanych ciągów. Dlatego też ważne jest, aby nie wykonywać konkatenacji w pętlach lub w miejscach, gdzie jest wykonywana duża ilość operacji na ciągach, ponieważ może to znacznie obciążyć pamięć. 

## Zobacz również

- [Dokumentacja PHP - konkatenacja](https://www.php.net/manual/en/language.operators.string.php)
- [Wideo-tutorial o konkatenacji w PHP](https://www.youtube.com/watch?v=4ZyAyTjEfHw)
- [Artykuł o optymalizacji konkatenacji w PHP](https://www.php-coding-practices.com/php-performance-concatenation/)
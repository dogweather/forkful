---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "PHP: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastępowanie tekstu jest kluczowym zagadnieniem w programowaniu, pozwalającym na automatyzację procesu edycji i aktualizacji tekstu w aplikacjach internetowych. Pozwala to zaoszczędzić dużo czasu i pracy, dlatego jest niezbędnym narzędziem dla każdego programisty.

## Jak to zrobić

Zastępowanie tekstu w PHP jest bardzo proste i wymaga wykorzystania funkcji `str_replace()`. Przyjmuje ona trzy parametry - pierwszym jest tekst, który chcemy zastąpić, drugim - tekst, którym chcemy go zastąpić, a trzecim - tekst, w którym chcemy dokonać zmian.

```PHP
$str = "Hello World!";
echo str_replace("World", "Universe", $str); // Output: Hello Universe!
```

Możemy również wykorzystać zmienne do przypisania nowego tekstu, co ułatwia jego późniejszą modyfikację.

```PHP
$search = "World";
$replace = "Universe";
$str = "Hello $search!";
echo str_replace($search, $replace, $str); // Output: Hello Universe!
```

Jeśli chcemy, aby zastępowanie odbyło się tylko raz, możemy użyć funkcji `str_replace_first()` lub `str_replace_last()`. Pierwsza z nich zastępuje tylko pierwsze wystąpienie, a druga - tylko ostatnie.

```PHP
$str = "John and Jane went to the park together.";
echo str_replace_first("Jane", "Maria", $str); // Output: John and Maria went to the park together.
echo str_replace_last("o", "i", $str); // Output: John and Jane went to the park together.
```

## Głębsza czerwień

Funkcja `str_replace()` nie jest jedynym sposobem na zastępowanie tekstu w PHP. Istnieje również funkcja `str_ireplace()`, która jest niewrażliwa na wielkość liter. Możemy również wykorzystać wyrażenia regularne do jeszcze bardziej zaawansowanego zastępowania tekstu.

```PHP
$str = "I love eating apples and bananas.";
echo preg_replace("/apples|bananas/i", "oranges", $str); // Output: I love eating oranges and oranges.
```

## Zobacz także

- [Dokumentacja PHP - funkcja str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [Wprowadzenie do wyrażeń regularnych w PHP](https://www.w3schools.com/php/php_regex.asp)
- [13 przydatnych funkcji PHP, o których prawdopodobnie nie słyszałeś](https://www.taniarascia.com/13-useful-php-functions-you-probably-havent-heard-of/)
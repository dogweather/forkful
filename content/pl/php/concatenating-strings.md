---
title:                "Łączenie ciągów znaków"
html_title:           "PHP: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Concatenacja w programowaniu jest nieodłącznym elementem tworzenia aplikacji internetowych. Jest to proces łączenia dwóch lub więcej ciągów znaków w jeden ciąg. Może to być przydatne w wielu przypadkach, takich jak wyświetlanie tekstu, łączenie zmiennych lub generowanie URL-ów.

## Jak

Przeprowadzenie operacji konkatenacji w PHP jest bardzo proste. Wystarczy użyć operatora kropki (.) do połączenia dwóch ciągów znaków. Przykładowo, jeśli chcielibyśmy wyświetlić tekst "Hello World", wykonajmy następujący kod:

```PHP
<?php
    $greeting = "Hello";
    $target = "World";
    echo $greeting . " " . $target;
?>
```

Output: `Hello World`

Możemy również łączyć zmienne i tekst w jednym ciągu. Na przykład:

```PHP
<?php
    $name = "John";
    echo "Hello, my name is " . $name . " and I am a PHP developer.";
?>
```

Output: `Hello, my name is John and I am a PHP developer.`

## Deep Dive

W języku PHP, ciąg znaków może być również konkatenowany za pomocą funkcji `sprintf()`. Funkcja ta przyjmuje dwa argumenty - pierwszy to szablon lub ciąg znaków z placeholderami, a drugi to lista zmiennych lub wartości, które zostaną wstawione w odpowiednie miejsca.

Przykładowo, jeśli chcielibyśmy wyświetlić tekst "I have 3 dogs and 2 cats", możemy użyć funkcji `sprintf()` w ten sposób:

```PHP
<?php
    $dogs = 3;
    $cats = 2;
    $text = sprintf("I have %d dogs and %d cats", $dogs, $cats);
    echo $text;
?>
```

Output: `I have 3 dogs and 2 cats`

Wartości mogą być również wstawiane w tekst za pomocą zmiennej używając symbolu `%s`. Na przykład:

```PHP
<?php
    $name = "Lisa";
    $text = sprintf("My name is %s and I have 2 children.", $name);
    echo $text;
?>
```

Output: `My name is Lisa and I have 2 children.`

Funkcja `sprintf()` jest szczególnie przydatna, gdy chcemy sformatować wyjście w określony sposób, na przykład wyświetlić dokładną liczbę miejsc po przecinku w liczbie zmiennoprzecinkowej.

## Zobacz również

- [PHP manual - String Operators](https://www.php.net/manual/en/language.operators.string.php)
- [PHP manual - sprintf() function](https://www.php.net/manual/en/function.sprintf.php)
- [PHP: The Right Way - Concatenation](https://phptherightway.com/#concatenation)
---
title:                "PHP: Zmiana wielkości liter w łańcuchu znaków"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego capitalizowanie tekstu może być ważne w programowaniu?

Czasami potrzebujemy, aby nasze teksty były zapisane zgodnie z określoną konwencją, taką jak na przykład w nazwie użytkownika czy też w tytule artykułu. Wtedy z pomocą przychodzi nam kapitalizowanie tekstu, czyli zamiana pierwszej litery na wielką.

## Jak to zrobić w PHP?

```PHP
$string = "paweł jest programistą";
$capitalized_string = ucwords($string);
echo $capitalized_string;
```
Ten kod powinien wyświetlić nam "Paweł Jest Programistą" jako wynik.

## Głębszy wgląd w mechanizm capitalizowania tekstu

Funkcja `ucwords()` w PHP działa poprzez podzielenie podanego tekstu na słowa i zmianę pierwszej litery każdego słowa na wielką. Należy jednak pamiętać, że funkcja ta zależy od ustawień lokalnych systemu, co może wpływać na wynik w różnych językach.

## Zobacz także

- [Dokumentacja funkcji ucwords() w PHP](https://www.php.net/manual/en/function.ucwords.php)
- [Poradnik o manipulacji tekstem w PHP](https://www.w3schools.com/php/php_string.asp)
- [Inne przydatne funkcje PHP do pracy z tekstem](https://www.codecademy.com/learn/learn-php/modules/learn-php-string-functions)
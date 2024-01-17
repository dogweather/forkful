---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "PHP: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja tekstu do małych liter to proces, w którym wszystkie znaki w danym ciągu są zamieniane na odpowiedniki z małych liter. Programiści często wykonują tę operację w celu ujednolicenia tekstu i ułatwienia porównywania ciągów znaków.

## Jak to zrobić?

Aby przekonwertować ciąg znaków na małe litery w PHP, można użyć funkcji `strtolower()`. Przykładowy kod wyglądałby następująco:

```
$string = "PRZYKŁADowy TEKST";
$converted_string = strtolower($string);
echo $converted_string;

// Output: przykładowy tekst
```

## Głębszy zanurzenie

Konwersja tekstu na małe litery jest często wykorzystywana do porównywania dwóch ciągów znaków bez względu na wielkość liter. Ułatwia to również sprawdzanie poprawności danych wprowadzanych przez użytkowników, ponieważ niektóre serwisy internetowe nie uwzględniają wielkości liter.

Alternatywą dla funkcji `strtolower()` jest `mb_strtolower()`, która jest bardziej wydajna przy obsłudze wielojęzycznych znaków. W celu ujednolicenia tekstu w całym dokumencie HTML, można użyć stylów CSS, takich jak `text-transform: lowercase`.

## Zobacz też

Dokumentacja PHP dotycząca funkcji `strtolower()`: https://www.php.net/manual/en/function.strtolower.php

Inne funkcje konwersji znaków w PHP: https://www.php.net/manual/en/ref.strings.php
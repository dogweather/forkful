---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---

# Konwersja ciągu na małe litery w PHP: Co, Dlaczego i Jak?

---

## Co i Dlaczego?

Konwersja ciągu na małe litery to technika, która zamienia wszystkie litery w danym ciągu na małe. Programiści robią to, by uniknąć problemów z wielkością liter, zwłaszcza przy porównywaniu ciągów.

---

## Jak to zrobić:

Oto krótki kod, który demonstruje, jak to zaimplementować w PHP:

```PHP
<?php
  $string = "Witaj, Świecie!";
  $lowerCaseString = strtolower($string);
  echo $lowerCaseString;
?>
```

Gdy uruchomisz powyższy kod, otrzymasz następujące wyniki:

```PHP
"witaj, świecie!"
```

Konwersja jest prosta i intuicyjna dzięki wbudowanej funkcji `strtolower()`.

---

## Bardziej szczegółowo:

Na przestrzeni lat funkcje konwersji liter na małe (i duże) znacząco ewoluowały. Wcześniejsze wersje PHP nie obsługiwały konwersji znaków specjalnych (np. alfabetu polskiego).

Alternatywą dla `strtolower()` jest funkcja `mb_strtolower()`, która jest lepsza przy obsłudze ciągów multibajtowych (np. niewielkich liter z polskim alfabetem).

Szczegóły implementacji: `strtolower()` przechodzi przez każdy znak w ciągu i zamienia go na małą literę. W PHP, `strtolower()` korzysta z ustawień lokalizacyjnych języka C.

---

## Zobacz też:

1. [Dokumentacja PHP dla strtolower()](https://www.php.net/manual/en/function.strtolower.php)
2. [Dokumentacja PHP dla mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)
3. [Porównanie mb_strtolower() i strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php#example-6535)

---
---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-01-20T17:39:09.138522-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? / Co i dlaczego?
Zamiana tekstu na małe litery to proces przekształcania wszystkich znaków w łańcuchu na ich odpowiedniki w dolnym rejestrowie. Programiści robią to dla ujednolicenia danych, ułatwienia porównywania stringów oraz obsługi wyszukiwania bez wielkości liter.

## How to: / Jak to zrobić:
W PHP możemy to zrobić używając funkcji `strtolower()`. Oto jak to działa:

```PHP
<?php
$originalString = "Hej! Jak się masz?";
$lowercaseString = strtolower($originalString);

echo $lowercaseString; // wyświetli: hej! jak się masz?
?>
```

Proste? Jak barszcz!

## Deep Dive / W głębię tematu:
Historia funkcji zmieniających wielkość liter w PHP sięga wersji PHP 4, gdzie `strtolower()` była już dostępna. Istnieją również alternatywy, takie jak `mb_strtolower()`, która dobrze radzi sobie z wielojęzycznymi łańcuchami znaków i obsługą różnych kodowań.

Warto zwrócić uwagę, że funkcja `strtolower()` może nie działać poprawnie z tekstami zawierającymi znaki spoza ASCII, np. polskie litery z ogonkami. Dlatego, gdy pracujesz z polskimi tekstami, sięgnij po `mb_strtolower()` i upewnij się, że ustawiłeś odpowiednie kodowanie, np. `mb_strtolower($string, 'UTF-8')`.

Dlaczego tak jest? Otóż PHP używa systemu kodowania znaków, a `strtolower` bazuje na standardzie ASCII. To może być nieadekwatne dla innych alfabetów. Mbstring (Multibyte String) rozszerzenie PHP jest uniwersalniejszą opcją.

```PHP
<?php
// Przykład użycia mb_strtolower():
$polishString = "A to Polska właśnie.";
$lowercasePolishString = mb_strtolower($polishString, 'UTF-8');

echo $lowercasePolishString; // wyświetli: a to polska właśnie.
?>
```

## See Also / Zobacz również:
- Oficjalna dokumentacja PHP dla `strtolower()`: https://www.php.net/manual/en/function.strtolower.php
- Oficjalna dokumentacja PHP dla `mb_strtolower()`: https://www.php.net/manual/en/function.mb-strtolower.php
- Tutorial dotyczący obsługi wielobajtowych łańcuchów znaków w PHP: https://www.php.net/manual/en/book.mbstring.php

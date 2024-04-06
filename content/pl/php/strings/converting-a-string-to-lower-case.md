---
date: 2024-01-20 17:39:09.138522-07:00
description: "How to: / Jak to zrobi\u0107: W PHP mo\u017Cemy to zrobi\u0107 u\u017C\
  ywaj\u0105c funkcji `strtolower()`. Oto jak to dzia\u0142a."
lastmod: '2024-04-05T22:37:44.182930-06:00'
model: gpt-4-1106-preview
summary: "/ Jak to zrobi\u0107: W PHP mo\u017Cemy to zrobi\u0107 u\u017Cywaj\u0105\
  c funkcji `strtolower()`. Oto jak to dzia\u0142a."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

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

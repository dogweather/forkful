---
date: 2024-01-20 17:46:14.672410-07:00
description: "How to: (Jak to zrobi\u0107:) W PHP wyci\u0105ganie podci\u0105g\xF3\
  w robi si\u0119 zazwyczaj za pomoc\u0105 funkcji `substr()`. Zosta\u0142a wprowadzona\
  \ w PHP 3 i od tego czasu jest\u2026"
lastmod: '2024-04-05T22:50:49.807034-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) W PHP wyci\u0105ganie podci\u0105g\xF3w robi si\u0119\
  \ zazwyczaj za pomoc\u0105 funkcji `substr()`."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## How to: (Jak to zrobić:)
```PHP
<?php
$text = "Witajcie w świecie PHP!";
$fragment = substr($text, 8, 7); // pobiera "w świecie"

echo $fragment; // wyświetli "w świecie"

// Inny przykład użycia substr bez długości
$koniec = substr($text, -4); // pobiera ostatnie 4 znaki "PHP!"

echo $koniec; // wyświetli "PHP!"
?>
```

## Deep Dive (W głąb tematu)
W PHP wyciąganie podciągów robi się zazwyczaj za pomocą funkcji `substr()`. Została wprowadzona w PHP 3 i od tego czasu jest podstawową metodą manipulacji ciągami. Alternatywy to funkcje takie jak `mb_substr()`, która lepiej radzi sobie z Unicode. `substr()` działa tak, że jako pierwszy argument przyjmuje ciąg źródłowy, jako drugi indeks startowy, a jako trzeci - opcjonalną długość podciągu do wyciągnięcia. Jeśli długość nie jest określona, `substr()` zwraca wszystko do końca ciągu.

## See Also (Zobacz również)
- Oficjalna dokumentacja PHP na temat funkcji `substr()`: [php.net/manual/en/function.substr.php](https://www.php.net/manual/en/function.substr.php)
- Porównanie `substr()` i `mb_substr()`: [www.php.net/manual/en/function.mb-substr.php](https://www.php.net/manual/en/function.mb-substr.php)
- Tutorial na temat manipulacji ciągami w PHP: [www.w3schools.com/php/php_ref_string.asp](https://www.w3schools.com/php/php_ref_string.asp)

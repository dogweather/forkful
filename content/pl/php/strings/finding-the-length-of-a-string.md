---
date: 2024-01-20 17:48:08.635290-07:00
description: "D\u0142ugo\u015B\u0107 \u0142a\u0144cucha znak\xF3w to po prostu liczba\
  \ znak\xF3w w nim zawartych. Programi\u015Bci sprawdzaj\u0105 t\u0119 d\u0142ugo\u015B\
  \u0107 by weryfikowa\u0107 dane wej\u015Bciowe, ogranicza\u0107 tekst do\u2026"
lastmod: '2024-03-13T22:44:35.487844-06:00'
model: gpt-4-1106-preview
summary: "D\u0142ugo\u015B\u0107 \u0142a\u0144cucha znak\xF3w to po prostu liczba\
  \ znak\xF3w w nim zawartych. Programi\u015Bci sprawdzaj\u0105 t\u0119 d\u0142ugo\u015B\
  \u0107 by weryfikowa\u0107 dane wej\u015Bciowe, ogranicza\u0107 tekst do\u2026"
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## Co i dlaczego?
Długość łańcucha znaków to po prostu liczba znaków w nim zawartych. Programiści sprawdzają tę długość by weryfikować dane wejściowe, ograniczać tekst do określonej długości czy po prostu manipulować treścią.

## Jak to zrobić:
W PHP używamy funkcji `strlen()` do uzyskania długości łańcucha znaków. Łatwo i szybko:

```PHP
<?php
$tekst = "Witaj, świecie!";
echo strlen($tekst);  // Wyświetli: 15
?>
```
Zwróć uwagę, że `strlen()` zwraca długość łańcucha dla jednobajtowych kodowań. Dla Unicode potrzebujemy `mb_strlen()`:

```PHP
<?php
$tekst = "Cześć";
echo mb_strlen($tekst, 'UTF-8');  // Wyświetli: 5
?>
```

## W głąb tematu
Wcześniejsze wersje PHP nie wspierały wielobajtowych kodowań - to wyzwanie w epoce globalizacji. Standard `strlen()` zakłada jeden bajt na znak, co jest niewystarczające dla języków używających alfabetów innych niż łaciński. Dlatego pojawiła się funkcja `mb_strlen()`, która obsługuje Unicode i może liczyć prawidłowo znaki w różnych kodowaniach (na przykład UTF-8).

Alternatywnie, z funkcji `iconv_strlen()` też można korzystać, by zmierzyć długość łańcucha w określonym kodowaniu.

A co z wydajnością? `strlen()` jest szybsza, bo działa na prostych, jednobajtowych znakach. `mb_strlen()` ma do wykonania więcej pracy, ale jest niezbędna, gdy pracujemy z tekstami w wielobajtowych kodowaniach, takich jak UTF-8.

## Zobacz również
- Dokumentacja PHP na temat funkcji `strlen()`: https://www.php.net/manual/pl/function.strlen.php
- Dokumentacja PHP na temat funkcji `mb_strlen()`: https://www.php.net/manual/pl/function.mb-strlen.php
- Porównanie różnych funkcji liczenia długości łańcuchów znaków: https://www.php.net/manual/pl/ref.mbstring.php

Pamiętaj, że wiedza o kodowaniach tekstów i właściwe ich użycie to podstawa przy pracy z różnorodnymi danymi wejściowymi. Nie lekceważ tego, szczególnie w aplikacjach międzynarodowych!

---
date: 2024-01-20 17:51:29.281309-07:00
description: "Jak to zrobi\u0107: Interpolowa\u0107 stringi mo\u017Cemy za pomoc\u0105\
  \ podw\xF3jnych cudzys\u0142ow\xF3w lub heredoc. Oto przyk\u0142ady."
lastmod: '2024-03-13T22:44:35.482473-06:00'
model: gpt-4-1106-preview
summary: "Interpolowa\u0107 stringi mo\u017Cemy za pomoc\u0105 podw\xF3jnych cudzys\u0142\
  ow\xF3w lub heredoc."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## Jak to zrobić:
Interpolować stringi możemy za pomocą podwójnych cudzysłowów lub heredoc. Oto przykłady:

```PHP
// Przykład z podwójnymi cudzysłowiami
$name = "Jan";
echo "Cześć, $name!";
// Wyświetli: Cześć, Jan!

// Przykład z heredoc
$car = "Fiat";
$owner = "Krzysztof";

$story = <<<EOT
Właścicielem auta marki $car jest $owner.
EOT;

echo $story;
// Wyświetli: Właścicielem auta marki Fiat jest Krzysztof.
```

## Głębsze spojrzenie:
Interpolacja stringów jest w PHP od zawsze — ułatwia łączenie zmiennych z tekstem. Alternatywą dla interpolacji jest konkatenacja za pomocą kropki (.), ale jest ona mniej wydajna i estetyczna w użyciu.

```PHP
// Konkatenacja - alternatywna metoda
$name = "Ania";
echo 'Cześć, ' . $name . '!';
// Wyświetli: Cześć, Ania!
```

Interpolacja działa tylko gdy string jest otoczony podwójnymi cudzysłowiami lub heredoc — w jednolitych cudzysłowach zmienne nie są interpretowane. Heredoc jest szczególnie przydatny przy bardzo długich tekstach, ponieważ zwiększa czytelność kodu.

## Zobacz również:
- Oficjalną dokumentację PHP na temat stringów: https://www.php.net/manual/pl/language.types.string.php
- Artykuł na temat różnic między podwójnymi a jednolitymi cudzysłowiami: https://www.php.net/manual/pl/language.types.string.php#language.types.string.syntax.double
- Poradnik na temat używania heredoc i nowdoc w PHP: https://www.php.net/manual/pl/language.types.string.php#language.types.string.syntax.heredoc

---
date: 2024-01-20 17:51:29.281309-07:00
description: "Interpolacja string\xF3w to wstawianie zmiennych do napis\xF3w w kodzie,\
  \ aby dynamicznie budowa\u0107 tekst. Programi\u015Bci u\u017Cywaj\u0105 jej, by\
  \ kod by\u0142 bardziej elastyczny\u2026"
lastmod: '2024-03-13T22:44:35.482473-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja string\xF3w to wstawianie zmiennych do napis\xF3w w kodzie,\
  \ aby dynamicznie budowa\u0107 tekst."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## Co i dlaczego?
Interpolacja stringów to wstawianie zmiennych do napisów w kodzie, aby dynamicznie budować tekst. Programiści używają jej, by kod był bardziej elastyczny i czytelny — zmienne mogą zmieniać treść napisu bez konieczności jego ciągłego przepisywania.

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

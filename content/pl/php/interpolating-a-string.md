---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:51:29.281309-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

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
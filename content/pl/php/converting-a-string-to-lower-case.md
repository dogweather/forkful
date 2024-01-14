---
title:                "PHP: Konwertowanie ciągu znaków na małe litery"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery jest ważną częścią programowania, ponieważ pozwala na jednolite porównywanie tekstów bez względu na to, czy są napisane wielkimi czy małymi literami. Jest to szczególnie przydatne przy porównywaniu danych, takich jak hasła, gdzie wielkość liter ma znaczenie.

## Jak to zrobić

Aby skonwertować string na małe litery w PHP, możesz skorzystać z funkcji `strtolower()`. Poniższy przykład pokazuje jak użyć tej funkcji:

```PHP
<?php
$string = "WITAJ W PROGRAMOWANIU W PHP!";
echo strtolower($string);
```

W powyższym kodzie, wartość zmiennej `$string` zostanie przekonwertowana na małe litery i wyświetlona na ekranie jako "witaj w programowaniu w php!".

## Głębsze spojrzenie

Funkcja `strtolower()` używa standardowego zestawu znaków do konwersji liter na małe. Jednak w przypadku języków, gdzie występują litery specjalne, może to powodować błędy. Aby uniknąć tego problemu, zaleca się użycie funkcji `mb_strtolower()`, która pozwala na dopasowanie zestawu znaków do odpowiedniego języka.

Ponadto, warto pamiętać, że funkcja `strtolower()` bezpośrednio modyfikuje wartość zmiennej, podczas gdy `mb_strtolower()` zwraca nowy string. Dlatego należy wybrać odpowiednią funkcję w zależności od potrzeb.

## Zobacz też

- [Funkcja strtolower() w dokumentacji PHP](https://www.php.net/manual/en/function.strtolower.php)
- [Funkcja mb_strtolower() w dokumentacji PHP](https://www.php.net/manual/en/function.mb-strtolower.php)
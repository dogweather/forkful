---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Łączenie ciągów znaków, znanego również jako konkatenacja, to proces spajania dwóch lub więcej ciągów znaków w jeden dłuższy. Programiści robią to, aby manipulować danymi i tworzyć dynamiczne ciągi, które są bardziej użyteczne i elastyczne.

## Jak to zrobić:

Łączenie ciągów w PHP jest proste. Używamy operatora kropki (.), aby to zrobić. W poniższym przykładzie łączymy dwa teksty.

```PHP
<?php
  $txt1 = "Witaj";
  $txt2 = "świecie!";
  echo $txt1 . " " . $txt2;
?>
```
Rezultatem tego kodu będzie:
```
Witaj świecie!
```

## Głębsze spojrzenie:

Konkatenacja ciągów znaków jest podstawą wielu języków programowania, a PHP nie jest wyjątkiem. W historii PHP, operator konkatenacji (.) jest już obecny od jego pierwszej wersji.

Istnieją alternatywne metody łączenia ciągów. Jednym z nich jest używanie funkcji `sprintf()`:

```PHP
<?php
  $txt1 = "Witaj";
  $txt2 = "świecie!";
  echo sprintf("%s %s", $txt1, $txt2);
?>
```
Rezultatem tego kodu będzie identyczny jak poprzedniego - `Witaj świecie!` .

Warto również pamiętać, że operacje łączenia ciągów mają pewne koszty wydajności, wszystko zależy od konkretnej implementacji i zastosowania.

## Zobacz też:

[Zasady programowania w PHP](https://php.net/manual/pl/)

[Łączenie ciągów w PHP](https://www.php.net/manual/pl/language.operators.string.php)

[Alternatywne metody łączenia ciągów w PHP](https://www.geekhideout.com/urlcode.php)
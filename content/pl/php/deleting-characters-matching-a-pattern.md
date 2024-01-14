---
title:                "PHP: Usuwanie znaków pasujących do wzoru"
simple_title:         "Usuwanie znaków pasujących do wzoru"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że podczas pisania kodu PHP potrzebujemy usunąć pewne znaki z tekstu, które pasują do pewnego wzorca. Niektóre funkcje PHP, takie jak `preg_replace()`, pozwalają na to w prosty sposób. Jednak jest wiele różnych metod, które mogą być użyte w zależności od potrzeb i sytuacji. W tym artykule pokażemy kilka sposobów na usuwanie znaków dopasowujących się do wybranego wzorca.

## Jak to zrobić

#### Przykład 1:

```PHP
<?php
$text = "Ten tekst zawiera <em>ważne</em> informacje.";
$pattern = "/<.*?>(.*?)<\/.*?>/i";
$replacement = "$1";
$new_text = preg_replace($pattern, $replacement, $text);

echo $new_text;
```

##### Output:

```
Ten tekst zawiera ważne informacje.
```

Powyższy przykład pokazuje użycie funkcji `preg_replace()` do usunięcia wszystkich znaczników HTML `<em>` i `</em>` z tekstu, pozostawiając tylko treść zawartą wewnątrz nich.

#### Przykład 2:

```PHP
<?php
$text = "12 stycznia";
$pattern = "/[0-9]/";
$replacement = "";
$new_text = preg_replace($pattern, $replacement, $text);

echo $new_text;
```

##### Output:

```
stycznia
```

W tym przykładzie użyliśmy funkcji `preg_replace()` z wyrażeniem regularnym, aby usunąć wszystkie cyfry z tekstu. Możemy również podać wiele różnych znaków do usunięcia, dodając je do nawiasów kwadratowych `[ ]` w wyrażeniu regularnym.

## Deep Dive

Istnieje wiele funkcji w PHP, które pozwalają na usuwanie znaków dopasowujących się do wybranego wzorca. Poniżej wymieniamy kilka z nich:

- `preg_replace()` - funkcja, która pozwala na zastosowanie wyrażenia regularnego do znalezienia i usunięcia dopasowujących znaków z tekstu.
- `str_replace()` - funkcja, która zastępuje podane znaki lub ciągi znaków innymi znakami lub ciągami znaków.
- `trim()` - funkcja, która usuwa białe znaki z początku i końca tekstu.
- `substr_replace()` - funkcja, która zastępuje ciąg znaków w określonym fragmencie tekstu.
- `mb_ereg_replace()` - funkcja podobna do `preg_replace()`, ale obsługuje tekst w wielu językach.

Ważne jest, aby wybrać odpowiednią funkcję w zależności od potrzeb i rodzaju tekstu, z którym mamy do czynienia.

## Zobacz także

- [Dokumentacja PHP - preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [10 Przykładów Używania Wyrażeń Regularnych w PHP](https://kursphp.com/przyklady/wyrazenia-regularne/)
- [Usuwanie znaków HTML z ciągu tekstowego w PHP](https://easyprogramming.net/deleting-html-tags-php/)
---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja daty na tekst umożliwia nam zapisanie tej daty w czytelniejszym formacie, który z łatwością można wyświetlić lub zapisywać. Programiści przeprowadzają tę operację, aby ułatwić prezentację danych datowych w human-friendly formie oraz aby ułatwić przechowywanie i manipulację tymi danymi.

## Jak to zrobić:

Użyjemy wbudowanej funkcji PHP o nazwie `date()`. Operuje ona na obiekcie `DateTime`.

`date()` przyjmuje dwa argumenty. Pierwszy to format, w jakim chcielibyśmy zobaczyć naszą datę. Drugi, który jest opcjonalny, to znacznik czasu Unix, który chcemy przekształcić.

```PHP
<?php

$date = new DateTime('2000-01-01');

echo $date->format('Y-m-d H:i:s') . "\n";
?>
```

To wygeneruje:

```
2000-01-01 00:00:00
```
## Głębsze zanurzenie:

Funkcję `date()` wprowadzono w PHP 4, a od tej pory stała się jednym z podstawowych narzędzi dla każdego programisty PHP. 

Alternatywą dla `date()` jest funkcja `gmdate()`, która działa dokładnie tak samo, z tym wyjątkiem, że zawsze zwraca czas GMT.

`DateTime::format '()` jest metodą obiektu `DateTime` wprowadzoną w PHP 5.2.0, która umożliwia konwertowanie daty na ciąg zgodnie z podanym formatem.

Wszystkie te funkcje korzystają z formatów opartych na tych używanych przez funkcję `strftime()` w języku C. Istnieje wiele różnych znaczników, które można zastosować, każdy reprezentujący różne elementy daty i czasu, na przykład `Y` dla pełnego roku, `m` dla miesiąca i `d` dla dnia.

## Zobacz też:

1. Dokumentacja PHP dla date() - https://www.php.net/manual/pl/function.date.php
2. Dokumentacja PHP dla gmdate() - https://www.php.net/manual/pl/function.gmdate.php
3. Dokumentacja PHP dla DateTime - https://www.php.net/manual/pl/class.datetime.php
4. Dokumentacja PHP dla DateTime::format() - https://www.php.net/manual/pl/datetime.format.php
5. Formaty języka C używane w PHP - https://www.php.net/manual/pl/function.strftime.php.
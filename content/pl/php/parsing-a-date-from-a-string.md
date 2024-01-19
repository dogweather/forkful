---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza daty ze stringa polega na konwersji danych tekstowych na format daty. Programiści wykonują to zadanie, aby można było manipulować i porównywać daty w przyjazny dla komputera sposób.

## Jak to zrobić:

Oto prosty przykład użycia wbudowanej funkcji PHP - `strtotime`.

```PHP
<?php
$data = "21-01-2022";
echo date("Y-m-d", strtotime($data));
?>
```
Wynik wyglądałby następująco:
```
2022-01-21
```
Ten kod konwertuje datę z formatu 'd-m-Y' na 'Y-m-d'.

## Głębsze spojrzenie:

Analiza daty ze stringa nie zawsze była łatwym zadaniem, ale dzięki nowoczessnym językom programowania jak PHP stało się to znacznie prostsze. 

Co do alternatyw, PHP posiada wiele funkcji do pracy z datami, np. `DateTime::createFromFormat`.

Szczegóły implementacji są znacznie bardziej złożone i obejmują wiele aspektów, takich jak obsługa stref czasowych, formatów dat etc.

## Zobacz także:

1. Dokumentacja PHP na temat `strtotime`: https://www.php.net/manual/pl/function.strtotime.
2. Dokumentacja PHP na temat `DateTime::createFromFormat`: https://www.php.net/manual/pl/datetime.createfromformat.php
3. Informacje na temat różnych formatów dat: https://www.php.net/manual/pl/datetime.format.php
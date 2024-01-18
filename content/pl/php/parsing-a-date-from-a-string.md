---
title:                "Analiza daty z ciągu znaków"
html_title:           "PHP: Analiza daty z ciągu znaków"
simple_title:         "Analiza daty z ciągu znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie daty z ciągu znaków to proces wyodrębniania daty z istniejącej postaci, na przykład daty w formacie tekstowym lub liczbowym. Programiści często dokonują tego, gdy chcą wyciągnąć i przekształcić potrzebne im informacje z licznych źródeł, takich jak pliki lub bazy danych.

## Jak to zrobić:

```PHP
<?php
$dateString = "25-10-2020";
$date = date_create_from_format("d-m-Y", $dateString);
echo date_format($date, "d/m/Y");
?>

// Output: 25/10/2020
```

## Głębokie zanurzenie:

Pierwsze narzędzia do parsowania dat zostały stworzone pod koniec lat 60. XX wieku, a od tego czasu metody i biblioteki się ewoluowały. Alternatywą dla wbudowanych w PHP funkcji jest użycie gotowych bibliotek, takich jak Carbon, która ułatwia pracę z datami i czasem w bardziej czytelny sposób. PHP posiada też możliwość wykorzystania funkcji z biblioteki standardowej C, takiej jak `strtotime()`, aby parsować daty w różnych językach. Implementacja parserów dat jest często związana z obsługą stref czasowych i róźnymi formatami dat.

## Zobacz też:

- Dokumentacja PHP dotycząca parsowania dat: https://www.php.net/manual/en/function.date-parse.php
- Biblioteka Carbon: https://carbon.nesbot.com/
- Porównanie różnych metod parsowania dat: https://www.php.net/manual/en/datetime.formats.comparison.php
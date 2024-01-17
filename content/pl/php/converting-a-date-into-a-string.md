---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "PHP: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
 Konwersja daty na łańcuch znaków oznacza przekształcenie daty i godziny, zapisanej w formacie ułatwiającym przetwarzanie przez komputer, na tekstowy format wyświetlany użytkownikowi. Programiści używają tego sposobu, aby dostosować wyświetlany czas do potrzeb użytkownika lub do formatowania na stronie internetowej.

## Jak to zrobić:
``` PHP
$timestamp = 1577854800;
echo date("d-m-Y", $timestamp);
```

**Output:**
01-01-2020

W powyższym przykładzie $timestamp oznacza liczbę sekund, która minęła od Początku Ery Unix (1 stycznia 1970 r.), a "d-m-Y" to format daty, w którym:
" d " oznacza dzień, " m " - miesiąc, a " Y " - rok.

``` PHP
echo date("l, F d, Y");
```
**Output:**
Wednesday, January 01, 2020

Powyższy kod nie wymaga podania timestamp, ponieważ automatycznie pobiera aktualną datę i godzinę.

## Głębsza analiza:
 Konwersja daty na łańcuch znaków jest często wykorzystywana w tworzeniu aplikacji internetowych, blogów, czy wszędzie tam, gdzie wyświetlanie aktualnej daty i godziny jest istotne. Alternatywą dla tej metody może być wykorzystanie gotowych bibliotek lub pluginów, które umożliwiają bardziej zaawansowane formatowanie daty oraz uwzględnienie stref czasowych.

## Zobacz także:
- Dokumentacja PHP: https://www.php.net/manual/en/function.date.php
- Porównanie różnych sposobów konwersji daty w PHP: https://www.tutorialrepublic.com/php-tutorial/php-working-with-date-and-time.php
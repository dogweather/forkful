---
title:                "Konwersja daty na łańcuch znaków"
date:                  2024-01-20T17:37:09.348411-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
W PHP konwersja daty do postaci łańcucha znaków pozwala na łatwe wyświetlanie i zapisywanie w różnych formatach. Robimy to często, by dostosować datę do preferencji użytkowników lub wymogów systemu.

## How to:
Użyj funkcji `date()` do formatowania i konwersji obiektu `DateTime` na łańcuch znaków.

```PHP
<?php
$date = new DateTime();
echo $date->format('Y-m-d H:i:s');
// Wyświetla: 2023-03-15 10:00:00

// Strefy czasowe
$date->setTimezone(new DateTimeZone('Europe/Warsaw'));
echo $date->format('Y-m-d H:i:s');
// Wyświetla: 2023-03-15 11:00:00
?>
```

Możesz również skorzystać z funkcji `strftime()` by dostosować format daty do ustawień lokalnych.

## Deep Dive
Przed PHP 8, `strftime()` był popularny w konwersji dat, ale został oznaczony jako przestarzały (deprecated). Od PHP 8, zalecane jest używanie `DateTime::format()`.

Dlaczego konwersja jest ważna? Dostosowanie formatu daty do lokalnych standardów pomaga w międzynarodowej obsłudze użytkowników. Umożliwia także łatwiejsze porównanie i sortowanie dat w bazach danych.

Alternatywą dla `DateTime::format()` jest `IntlDateFormatter` z rozszerzenia Internationalization (intl), które pozwala na jeszcze bardziej elastyczne formatowanie dat z uwzględnieniem lokalizacji.

Implementacja `DateTime::format()` wykorzystuje wewnętrzne klasy C w PHP, co zapewnia wydajność i dokładność w przetwarzaniu dat.

## See Also
- Dokumentacja PHP na temat DateTime: https://www.php.net/manual/en/class.datetime.php
- Informacje o funkcji strftime() i jej przestarzałości: https://www.php.net/manual/en/function.strftime.php
- Przykłady IntlDateFormatter: https://www.php.net/manual/en/class.intldateformatter.php
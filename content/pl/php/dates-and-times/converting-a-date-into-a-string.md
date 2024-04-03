---
date: 2024-01-20 17:37:09.348411-07:00
description: "How to: U\u017Cyj funkcji `date()` do formatowania i konwersji obiektu\
  \ `DateTime` na \u0142a\u0144cuch znak\xF3w."
lastmod: '2024-03-13T22:44:35.508931-06:00'
model: gpt-4-1106-preview
summary: "U\u017Cyj funkcji `date()` do formatowania i konwersji obiektu `DateTime`\
  \ na \u0142a\u0144cuch znak\xF3w."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

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

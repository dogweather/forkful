---
title:                "Pobieranie bieżącej daty"
html_title:           "PHP: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?

Pobieranie bieżącej daty jest częstym zadaniem dla programistów. Polega ono na uzyskaniu aktualnego czasu, daty i strefy czasowej. Jest to przydatne w wielu aplikacjach internetowych, takich jak rezerwacja biletów, harmonogramowanie spotkań czy wyświetlanie informacji o dostępności produktów.

## Jak to zrobić:

```PHP
<?php
echo date("d/m/Y"); // Dziś jest 13/11/2020
echo date("l"); // Dziś jest piątek
echo date("H:i:s"); // Aktualna godzina: 14:30:00
?>
```

## Głębsze rozeznanie:

Pobieranie bieżącej daty jest możliwe dzięki wbudowanej funkcji `date()` w PHP. Pierwszy parametr tej funkcji określa format w jakim chcemy otrzymać datę, a drugi opcjonalny parametr wskazuje na konkretny czas, dla którego chcemy uzyskać datę. Istnieje również alternatywna funkcja `time()` pozwalająca na uzyskanie bieżącego czasu w formie "unix timestamp". W przypadku potrzeby bardziej zaawansowanego manipulowania datami, można skorzystać z klas `DateTime` lub `Carbon`.

## Zobacz także:

- [Dokumentacja PHP o funkcji `date()`](https://www.php.net/manual/en/function.date.php)
- [Informacje o klasie DateTime w PHP](https://www.php.net/manual/en/class.datetime.php)
- [Strona z dokumentacją klasy Carbon](https://carbon.nesbot.com/docs/)
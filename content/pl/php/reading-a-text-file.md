---
title:                "Odczytywanie pliku tekstowego"
aliases:
- pl/php/reading-a-text-file.md
date:                  2024-01-20T17:54:57.683300-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Czytanie pliku tekstowego to proces pobierania danych zawartych w pliku tekstowym do dalszego przetwarzania. Programiści robią to, by obsługiwać konfiguracje, importować dane lub po prostu wyświetlać tekst użytkownikowi.

## Jak to zrobić:
```PHP
<?php
// Otwieramy plik 'przykladowy.txt' w trybie tylko do odczytu
$plik = fopen('przykladowy.txt', 'r');

// Czytamy całą zawartość pliku do zmiennej $zawartosc
$zawartosc = fread($plik, filesize('przykladowy.txt'));

// Wyświetlamy zawartość
echo $zawartosc;

// Zamykamy plik
fclose($plik);
?>
```

## Deep Dive
Historia czytania plików tekstowych w językach programowania jest tak stara jak same języki. Alternatywą dla `fopen()` i `fread()` w PHP jest użycie funkcji `file_get_contents()`, która skróci kod:

```PHP
<?php
$zawartosc = file_get_contents('przykladowy.txt');
echo $zawartosc;
?>
```

Ale czym się różnią te metody? Używając `fopen()`, możemy precyzyjnie kontrolować, jak dużo danych czytamy z pliku oraz reagować na błędy podczas otwierania pliku. `file_get_contents()` jest proste i szybkie, ale mniej elastyczne.

## Zobacz również
- Tutorial do głębszego zanurzenia się w temat: [PHP: The Right Way](https://phptherightway.com/#files)

---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie plików tekstowych to proces odzyskiwania danych zapisanych w zasobie tekstowym. Programiści robią to, aby uzyskać informacje przechowywane poza aplikacją, które mogą być użyte do różnych celów, takich jak analiza, manipulacja danych i wiele innych.

## Jak to zrobić:

Użycie wbudowanej w PHP funkcji `file_get_contents` jest jednym ze sposobów na odczytanie zawartości pliku tekstowego. Oto przykładowy kod i wynik:

```PHP
<?php
$filename = 'example.txt';
$filedata = file_get_contents($filename);
echo $filedata;
?>
```

The output:

```PHP
'Hello World!'
```

## Pogłębiona analiza

Czytanie plików tekstowych była jedną z pierwszych operacji, które starożytne komputery mogły wykonywać. Historia rozpoczęła się od kart perforowanych, które zawierały informacje w postaci otworów w konkretnych miejscach.

Alternatywą dla funkcji `file_get_contents` jest użycie funkcji `fread()` po otwarciu pliku za pomocą `fopen()`. Jednak `file_get_contents` jest zdecydowanie prostsza do użycia, szczególnie dla początkujących.

Podczas odczytywania dużych plików tekstowych, trzeba zachować ostrożność, ponieważ `file_get_contents` ładuje cały plik do pamięci. Dla bardzo dużych plików, może to prowadzić do problemów z wydajnością.

## Zobacz również

- [Oficjalna dokumentacja PHP na temat odczytywania plików](https://www.php.net/manual/en/function.file-get-contents.php)
- [Alternatywne metody odczytywania plików w PHP](https://www.php.net/manual/en/function.fread.php)
- [Historia i kontekst czytania plików](https://en.wikipedia.org/wiki/History_of_computer_data_storage)
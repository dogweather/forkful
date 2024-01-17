---
title:                "Praca z plikami csv"
html_title:           "PHP: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obsługa plików CSV to niezbędne narzędzie dla programistów PHP. CSV, czyli Comma Separated Values, jest formatem zapisu tabeli, w którym dane są rozdzielone przecinkami. Programiści używają go do przechowywania dużych ilości informacji w prosty i czytelny sposób.

## Jak to zrobić:

Aby pracować z plikami CSV w PHP, musimy użyć funkcji ```fgetcsv()```, która czyta kolejne linie z pliku CSV i zwraca je w formie tablicy. Przykład:

```php
$file = fopen("test.csv", "r"); // otwarcie pliku w trybie do odczytu
if ($file !== false) { // sprawdzenie, czy plik został otwarty poprawnie
  while (($data = fgetcsv($file, 1000, ",")) !== false) { // czytanie linii z pliku
    print_r($data); // wyświetlenie zawartości linii w postaci tablicy
  }
  fclose($file); // zamknięcie pliku
}
```

Powyższy kod będzie czytał kolejne linie z pliku ```test.csv``` i wyświetlał je na ekranie w formie tablicy. Ważne jest, aby przy użyciu funkcji ```fgetcsv()``` podać trzy argumenty: uchwyt do pliku, maksymalną długość linii oraz znak, którym są oddzielone wartości w pliku (w tym przypadku przecinek).

Aby zapisać dane do pliku CSV, możemy użyć funkcji ```fputcsv()```, która zapisuje podaną tablicę z danymi do pliku. Przykład:

```php
$file = fopen("test.csv", "w"); // otwarcie pliku w trybie do zapisu
$data = array("John", "Doe", "35"); // przykładowe dane do zapisania
fputcsv($file, $data); // zapisanie danych do pliku
fclose($file); // zamknięcie pliku
```

Powyższy kod utworzy nowy plik ```test.csv``` i zapisze do niego dane w postaci oddzielonych przecinkami wartości.

## Zanurzanie się w temat:

Plik CSV został stworzony w latach 70. XX wieku jako prosty format zapisu danych tabelarycznych. Obecnie jest on jednym z najbardziej popularnych formatów plików, wykorzystywanym przez wiele programów do importu i eksportu danych.

Alternatywą dla plików CSV są bazy danych, które oferują bardziej rozbudowane możliwości przechowywania i manipulowania danymi. Jednak w przypadku małych i prostych projektów, pliki CSV są wystarczające i łatwiejsze w obsłudze.

W PHP istnieje również funkcja ```str_getcsv()```, która pozwala na bezpośrednie przetworzenie ciągu znaków z danymi w formacie CSV, bez potrzeby otwierania pliku.

## Zobacz także:

- Dokumentacja PHP dotycząca funkcji ```fgetcsv()``` i ```fputcsv()```: https://www.php.net/manual/en/function.fgetcsv.php, https://www.php.net/manual/en/function.fputcsv.php
- Opis formatu CSV: https://tools.ietf.org/html/rfc4180
- Poradnik o pracy z plikami CSV w PHP: https://www.phpzag.com/how-to-read-and-write-csv-file-using-php/
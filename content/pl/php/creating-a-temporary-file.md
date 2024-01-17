---
title:                "Tworzenie tymczasowego pliku"
html_title:           "PHP: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?

Tworzenie pliku tymczasowego to proces, w którym programista tworzy tymczasowy plik na potrzeby wykonywania określonego zadania. Jest to często wykorzystywane w aplikacjach webowych, aby tymczasowo przechowywać dane lub wykonane operacje.

## Jak to zrobić?

```PHP
$tempFile = tempnam(sys_get_temp_dir(), 'prefix_');
```
W powyższym przykładzie tworzymy tymczasowy plik przy użyciu funkcji tempnam(). Pierwszy argument to ścieżka do katalogu tymczasowego na serwerze, a drugi to prefiks, który będzie wykorzystany do nazwy pliku.

```PHP
$fileContent = "This is a sample text.";
$tempFile = tempnam(sys_get_temp_dir(), 'prefix_');
file_put_contents($tempFile, $fileContent);
echo file_get_contents($tempFile);
```
W tym przykładzie do pliku tymczasowego zapisujemy zawartość tekstu, a następnie ją odczytujemy i wyświetlamy.

## Wnikliwe spojrzenie

Tworzenie pliku tymczasowego jest powszechną praktyką w programowaniu, która ma swoje korzenie jeszcze z czasów, gdy serwery były mniej wydajne. Wtedy tworzenie pliku tymczasowego było wykorzystywane w celu zapisania danych, które nie były jeszcze gotowe do finalnego zapisania na serwerze.

Alternatywą dla tworzenia plików tymczasowych może być wykorzystanie baz danych lub pamięci podręcznej. Jednak w niektórych przypadkach, tworzenie tymczasowych plików jest szybsze i wygodniejsze.

Tworzenie pliku tymczasowego w PHP jest możliwe dzięki funkcji tempnam(). Możliwe jest również wykorzystanie funkcji tmpfile(), która zwraca uchwyt do otwartego pliku tymczasowego. Istotną kwestią jest również usunięcie pliku tymczasowego po wykonaniu wszystkich operacji na nim, aby nie pozostawić niepotrzebnych śmieci w systemie.

## Zobacz także

- [PHP - Dokumentacja o funkcji tempnam()](https://www.php.net/manual/en/function.tempnam.php)
- [PHP - Dokumentacja o funkcji tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [Tworzenie plików tymczasowych w PHP - poradnik](https://www.sitepoint.com/temporary-files-in-php/)
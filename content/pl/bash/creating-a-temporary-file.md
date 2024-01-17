---
title:                "Tworzenie tymczasowego pliku"
html_title:           "Bash: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie pliku tymczasowego to proces tworzenia pliku, który jest używany tylko do przechowywania danych tymczasowych lub do wykonania określonego zadania. Programiści często tworzą pliki tymczasowe, aby przechowywać informacje, które są potrzebne tylko w określonym momencie lub których nie chcą zachowywać w stałym pliku.

## Jak to zrobić?

W Bash tworzenie pliku tymczasowego można zrobić za pomocą polecenia "mktemp". Polecenie to tworzy pusty plik o losowej nazwie w bieżącym katalogu. Można również utworzyć plik tymczasowy o określonej nazwie za pomocą opcji "-p". Przykładowy kod:

```Bash
# tworzenie pustego pliku tymczasowego
mktemp
# tworzenie pliku tymczasowego o nazwie "temp_file"
mktemp -p temp_file
```

Przykładowy output:

```Bash
$ mktemp
/private/var/folders/bm/368vf7kn7l14gldsnc172wsw0000gp/T/tmp.tPOqEAol

$ mktemp -p temp_file
temp_filehshLSr9
```

## Głębsza analiza

Tworzenie plików tymczasowych jest popularną praktyką w świecie programowania. Pierwsze wykorzystanie tego sposobu datuje się na lata 70., kiedy to mało dostępna była pamięć komputera i nie można było trzymać dużych danych w pamięci. Obecnie istnieją również inne sposoby na przechowywanie danych tymczasowych, na przykład w zmiennej środowiskowej USER_TEMPDIR. Jednakże tworzenie plików tymczasowych jest w dalszym ciągu stosowane ze względu na swoją prostotę i efektywność.

## Zobacz również

- [Bash man page](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Tutorial Bash dla początkujących](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [Poradnik tworzenia plików tymczasowych w innych językach programowania](https://www.tecmint.com/create-temporary-files-and-directories-in-linux/)
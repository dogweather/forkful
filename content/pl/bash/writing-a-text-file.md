---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie pliku tekstowego to zapisywanie danych w formie czytelnej dla człowieka. Programiści robią to, by przechowywać konfiguracje, logi lub wymieniać informacje między programami.

## How to:
Tworzenie pliku tekstowego za pomocą `echo`:
```Bash
echo "Cześć, to Twoja pierwsza linijka tekstu" > przykladowy_plik.txt
```
Dodawanie treści do istniejącego pliku:
```Bash
echo "To kolejna linia tekstu" >> przykladowy_plik.txt
```
Wyświetlenie zawartości pliku:
```Bash
cat przykladowy_plik.txt
```
Oczekiwane wyjście:
```
Cześć, to Twoja pierwsza linijka tekstu
To kolejna linia tekstu
```

## Deep Dive
Pisanie do plików tekstowych w Bashu sięga korzeniami pierwszych wersji Unix i jest podstawą wielu skryptów. Alternatywy `echo` do zapisu to `printf` lub `tee`. Szczegół implementacyjny: `>` nadpisuje plik, `>>` dołącza do istniejącej zawartości.

## See Also
- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial)
- [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/)
- [GNU Bash documentation](https://www.gnu.org/software/bash/manual/)

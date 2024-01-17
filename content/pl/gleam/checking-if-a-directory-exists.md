---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "Gleam: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Sprawdzanie, czy katalog istnieje, oznacza po prostu sprawdzenie, czy dana ścieżka w systemie plików jest katalogiem. Programiści tego dokonują, aby upewnić się, czy kod będzie działał poprawnie, jeśli mamy do czynienia z katalogiem.

## Jak to zrobić:
```Gleam
import gleam/path
import gleam/fs

path := "/home/"
fs.exists(path) // zwraca True jeśli istnieje, False jeśli nie
```

Przykładowe wyjście:
```
False
```

## Głębsza analiza:
(1) Sprawdzanie istnienia katalogu jest ważnym procesem w programowaniu, ponieważ daje nam pewność, że kod działa poprawnie. (2) Istnieje również kilka alternatyw do Gleam, takich jak wtyczki do systemów plików lub biblioteki oferowane przez inne języki programowania. (3) Implementacja sprawdzania istnienia katalogu w Gleam opiera się na wykorzystaniu funkcji systemowych dostępnych w danym systemie operacyjnym.

## Zobacz także:
- [Dokumentacja Gleam o sprawdzania istnienia pliku](https://gleam.run/time/)
- [Podręcznik Gleam o systemie plików](https://gleam.run/io/)
- [Inne języki programowania, które oferują funkcje do sprawdzania istnienia pliku](https://stackoverflow.com/questions/416556/using-python-how-can-i-check-if-a-file-exists-in-the-current-directory/927794#927794)
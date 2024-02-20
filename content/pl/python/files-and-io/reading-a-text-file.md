---
date: 2024-01-20 17:55:15.600877-07:00
description: "Czytanie pliku tekstowego to proces wyci\u0105gania informacji z pliku\
  \ zapisanego na dysku. Programi\u015Bci robi\u0105 to, by manipulowa\u0107 danymi,\
  \ wy\u015Bwietla\u0107 je\u2026"
lastmod: 2024-02-19 22:04:54.151484
model: gpt-4-1106-preview
summary: "Czytanie pliku tekstowego to proces wyci\u0105gania informacji z pliku zapisanego\
  \ na dysku. Programi\u015Bci robi\u0105 to, by manipulowa\u0107 danymi, wy\u015B\
  wietla\u0107 je\u2026"
title: Odczytywanie pliku tekstowego
---

{{< edit_this_page >}}

## Co i Dlaczego?
Czytanie pliku tekstowego to proces wyciągania informacji z pliku zapisanego na dysku. Programiści robią to, by manipulować danymi, wyświetlać je użytkownikowi, lub wczytywać konfiguracje swoich aplikacji.

## Jak to zrobić:
```Python
# Przykład 1: Odczytanie całego pliku

with open('przykladowy.txt', 'r', encoding='utf-8') as plik:
    zawartosc = plik.read()
print(zawartosc)

# Przykład 2: Odczytanie linijka po linijce

with open('przykladowy.txt', 'r', encoding='utf-8') as plik:
    for linijka in plik:
        print(linijka.strip())
```

Wynik (dla obu przykładów, zakładając identyczną zawartość pliku `przykladowy.txt`):
```
Pierwsza linijka tekstu
Druga linijka tekstu
Trzecia linijka tekstu
```

## Głębsze spojrzenie:
Odczyt plików tekstowych jest jak chleb powszedni w programowaniu. Python od swojego początku proponuje wbudowane funkcje do obsługi plików, a jego filozofia "Batteries included" oznacza, że wszystko jest pod ręką.

Wcześniej użycie trybu 'r' w funkcji `open` było standardem, ale nie zawsze obsługiwało poprawnie Unicode. Od Pythona 3.x, zalecane jest jawne określenie kodowania, np. `utf-8`.

Alternatywy? Możesz użyć modułu `io` dla zaawansowanych możliwości lub `pandas` do wczytywania tabelarycznych danych.

Szczegóły implementacji? W Pythonie plik jest obiektem, który ma swoje metody, np. `.read()`, `.readline()` czy `.readlines()`, dostosowane do różnych potrzeb. Otwarcie pliku za pomocą `with` zapewnia jego prawidłowe zamknięcie po zakończeniu operacji.

## Zobacz także:
- [Dokumentacja Pythona dla `open`](https://docs.python.org/3/library/functions.html#open)
- [Python Input and Output](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Moduł `io` w Pythonie](https://docs.python.org/3/library/io.html)
- [Moduł `pandas` - wczytywanie danych](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.read_csv.html)

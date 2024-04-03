---
date: 2024-01-20 17:55:15.600877-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:34.968080-06:00'
model: gpt-4-1106-preview
summary: .
title: Odczytywanie pliku tekstowego
weight: 22
---

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

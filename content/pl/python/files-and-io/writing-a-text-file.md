---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:22.215750-07:00
description: "Jak to zrobi\u0107: Wbudowana funkcja `open()` w Pythonie jest najcz\u0119\
  stsz\u0105 metod\u0105 zapisu do plik\xF3w. Funkcja pozwala na okre\u015Blenie trybu,\
  \ w jakim plik jest\u2026"
lastmod: '2024-03-13T22:44:34.969046-06:00'
model: gpt-4-0125-preview
summary: "Wbudowana funkcja `open()` w Pythonie jest najcz\u0119stsz\u0105 metod\u0105\
  \ zapisu do plik\xF3w."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:


### Korzystając z wbudowanej funkcji `open()`
Wbudowana funkcja `open()` w Pythonie jest najczęstszą metodą zapisu do plików. Funkcja pozwala na określenie trybu, w jakim plik jest otwierany - 'w' dla zapisu (nadpisywania), 'a' dla dodawania do końca i 'w+' dla zapisu+odczytu.

```python
# Zapis do nowego pliku lub zamiana istniejącego pliku
with open('example.txt', 'w') as file:
    file.write("Witaj, świecie!\n")

# Dodawanie do pliku
with open('example.txt', 'a') as file:
    file.write("Dodaję więcej tekstu.\n")

# Odczyt pliku do weryfikacji
with open('example.txt', 'r') as file:
    print(file.read())
```
**Przykładowy wynik:**
```
Witaj, świecie!
Dodaję więcej tekstu.
```

### Korzystając z `pathlib.Path`
Dla bardziej zorientowanego obiektowo podejścia, klasa `Path` z modułu `pathlib` oferuje metodę zapisu do plików. Jest to popularna metoda w nowszych bazach kodu Pythona.

```python
from pathlib import Path

# Zapis/Zamiana pliku
Path('example2.txt').write_text("To jest przykład 2.\n")

# Odczyt pliku do weryfikacji
print(Path('example2.txt').read_text())

# Uwaga: `Path.write_text` zawsze nadpisuje zawartość pliku.
# Do dodawania, należy otworzyć plik jak pokazano w poprzedniej sekcji.
```
**Przykładowy wynik:**
```
To jest przykład 2.
```

### Biblioteki stron trzecich
Do złożonych operacji na plikach, biblioteki stron trzecich, takie jak `pandas` (dla plików CSV, Excel), mogą być świetnym atutem. Oto krótki przykład zapisu DataFrame do pliku CSV za pomocą `pandas`, demonstrujący jego użyteczność poza prostymi plikami tekstowymi.

```python
# Ten przykład wymaga pandas: pip install pandas
import pandas as pd

# Tworzenie prostego DataFrame
data = pd.DataFrame({'Kolumna1': [1, 2, 3], 'Kolumna2': ['A', 'B', 'C']})

# Zapis DataFrame do pliku CSV
data.to_csv('przyklad.csv', index=False)

# Odczyt CSV do weryfikacji
print(pd.read_csv('przyklad.csv'))
```
**Przykładowy wynik:**
```
   Kolumna1 Kolumna2
0         1        A
1         2        B
2         3        C
```

Korzystając z tych metod, programiści Python mogą skutecznie zarządzać operacjami na plikach, zaspokajając potrzeby zarówno proste, jak i złożone przetwarzania danych.

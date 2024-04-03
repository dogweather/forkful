---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:45.511560-07:00
description: "Jak to zrobi\u0107: **Czytanie pliku CSV linia po linii**."
lastmod: '2024-03-13T22:44:35.607816-06:00'
model: gpt-4-0125-preview
summary: '**Czytanie pliku CSV linia po linii**.'
title: Praca z plikami CSV
weight: 37
---

## Jak to zrobić:
**Czytanie pliku CSV linia po linii**

```bash
while IFS=, read -r kolumna1 kolumna2 kolumna3
do
  echo "Kolumna 1: $kolumna1, Kolumna 2: $kolumna2, Kolumna 3: $kolumna3"
done < przykładowy.csv
```

*Przykładowe wyjście:*

```
Kolumna 1: id, Kolumna 2: nazwa, Kolumna 3: email
...
```

**Filtrowanie wierszy CSV na podstawie warunku**

Używając `awk`, możesz łatwo filtrować wiersze. Na przykład, aby znaleźć wiersze, gdzie druga kolumna równa się "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' przykładowy.csv
```

**Modyfikacja wartości kolumny**

Aby zmienić wartość drugiej kolumny na wielkie litery:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' przykładowy.csv
```

**Sortowanie pliku CSV na podstawie kolumny**

Możesz sortować plik CSV na podstawie na przykład trzeciej kolumny (numerycznie):

```bash
sort -t, -k3,3n przykładowy.csv
```

**Użycie `csvkit` do bardziej złożonych zadań**

`csvkit` to zbiór narzędzi wiersza poleceń do konwersji i pracy z plikami CSV. Może być zainstalowany za pomocą pip.

Aby przekonwertować plik JSON na CSV:

```bash
in2csv dane.json > dane.csv
```

Aby zapytać plik CSV przy użyciu SQL:

```bash
csvsql --query "SELECT nazwa FROM przykładowy WHERE id = 10" przykładowy.csv
```

*Uwaga: Instalowanie `csvkit` wymaga Pythona i może być wykonane za pomocą `pip install csvkit`.*

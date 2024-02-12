---
title:                "Praca z plikami CSV"
aliases:
- /pl/bash/working-with-csv.md
date:                  2024-02-03T19:18:45.511560-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z plikami CSV (Comma-Separated Values - wartości rozdzielone przecinkami) w Bashu polega na przetwarzaniu i manipulowaniu danymi tabelarycznymi przechowywanymi w formacie tekstu zwykłego. Jest to kluczowe dla programistów, ponieważ pozwala na automatyzację zadań transformacji, analizy i integracji danych bezpośrednio z linii poleceń, bez potrzeby korzystania z bardziej zaawansowanych narzędzi czy środowisk programistycznych.

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

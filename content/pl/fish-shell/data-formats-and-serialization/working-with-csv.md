---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:44.756348-07:00
description: "Jak to zrobi\u0107: Sam Fish Shell nie ma wbudowanych funkcji specjalnie\
  \ przeznaczonych do manipulacji plikami CSV. Jednak\u017Ce mo\u017Cna wykorzysta\u0107\
  \ narz\u0119dzia\u2026"
lastmod: '2024-03-13T22:44:35.864186-06:00'
model: gpt-4-0125-preview
summary: Sam Fish Shell nie ma wbudowanych funkcji specjalnie przeznaczonych do manipulacji
  plikami CSV.
title: Praca z plikami CSV
weight: 37
---

## Jak to zrobić:
Sam Fish Shell nie ma wbudowanych funkcji specjalnie przeznaczonych do manipulacji plikami CSV. Jednakże można wykorzystać narzędzia Unixowe takie jak `awk`, `sed` i `cut` do podstawowych operacji lub użyć specjalistycznych narzędzi takich jak `csvkit` do zadań bardziej zaawansowanych.

### Odczytywanie pliku CSV i wyświetlanie pierwszej kolumny:
Użycie `cut` do ekstrakcji pierwszej kolumny:
```fish
cut -d ',' -f1 data.csv
```
Przykładowe wyjście:
```
Name
Alice
Bob
```

### Filtrowanie wierszy CSV na podstawie wartości kolumny:
Użycie `awk` do znalezienia wierszy, gdzie druga kolumna odpowiada "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Przykładowe wyjście:
```
Bob,42,London
```

### Modyfikowanie pliku CSV (np. dodanie kolumny):
Użycie `awk` do dodania kolumny ze stałą wartością "NewColumn":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
Przykładowe wyjście w `modified.csv`:
```
Name,Age,City,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### Używanie `csvkit` do bardziej zaawansowanych operacji:
Najpierw upewnij się, że masz zainstalowany `csvkit`. Jeśli nie, zainstaluj go używając pip: `pip install csvkit`.

**Konwertowanie pliku CSV na JSON:**
```fish
csvjson data.csv > data.json
```
Przykładowe wyjście `data.json`:
```json
[{"Name":"Alice","Age":"30","City":"New York"},{"Name":"Bob","Age":"42","City":"London"}]
```

**Filtrowanie za pomocą `csvgrep` z `csvkit`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
To polecenie powtarza zadanie filtrowania, ale używając `csvkit`, kierując się na kolumnę 2 dla wartości "42".

Podsumowując, chociaż sam Fish Shell może nie oferować bezpośrednich możliwości manipulacji plikami CSV, jego bezproblemowa integracja z narzędziami Unixowymi i dostępność narzędzi takich jak `csvkit` zapewniają potężne opcje do pracy z plikami CSV.

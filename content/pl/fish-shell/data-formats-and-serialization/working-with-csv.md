---
title:                "Praca z plikami CSV"
aliases:
- /pl/fish-shell/working-with-csv.md
date:                  2024-02-03T19:19:44.756348-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV (Comma Separated Values), czyli wartościami rozdzielanymi przecinkami, wiąże się z parsowaniem, manipulowaniem i generowaniem danych w formacie tabelarycznym, który jest szeroko stosowany do wymiany danych między aplikacjami. Programiści wykonują te operacje, aby efektywnie przetwarzać i analizować dane, automatyzować zadania lub integrować się z innymi systemami.

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

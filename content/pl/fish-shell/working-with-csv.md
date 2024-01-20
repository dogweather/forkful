---
title:                "Praca z plikami CSV"
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Praca z CSV to przetwarzanie plików tekstowych zawierających dane oddzielone przecinkami. Programiści robią to, aby łatwo wymieniać dane między różnymi systemami i aplikacjami.

## How to: (Jak to zrobić:)
```Fish Shell
# Czytanie pliku CSV i wyświetlanie pierwszej kolumny
cat dane.csv | string split -r ',' -- | read -la columns
echo $columns[1]

# Zapisywanie określonej kolumny do nowego pliku CSV
cat dane.csv | string split -r ',' -- | read -la columns
echo $columns[2] > nowa_kolumna.csv

# Sortowanie danych CSV po drugiej kolumnie i wyświetlanie wyników
cat dane.csv | sort --field-separator=',' --key=2
```

## Deep Dive (Dogłębna analiza)
CSV (Comma-Separated Values) to format używany od dawna ze względu na prostotę. Istnieją alternatywy jak JSON czy XML, ale CSV pozostaje popularny dla prostych tabel danych. Przetwarzanie CSV w Fish nie wymaga zewnętrznych narzędzi, lecz dla skomplikowanych zadań lepiej użyć dedykowanych programów jak `xsv` lub `csvkit`.

## See Also (Zobacz również)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [xsv GitHub Repository](https://github.com/BurntSushi/xsv)
- [csvkit Documentation](https://csvkit.readthedocs.io/en/latest/)
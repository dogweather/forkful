---
title:                "Praca z plikami csv"
html_title:           "Bash: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

CSV to format pliku, który jest często używany przez programistów do przechowywania i przetwarzania danych tabelarycznych. Jest to skrót od angielskiego terminu "Comma Separated Values" (wartości oddzielane przecinkami). Ułatwia on przechowywanie danych w formacie tekstowym, co ułatwia czytanie i edycję dla programistów.

## Jak to zrobić?

Aby pracować z plikami CSV w Bash, możemy skorzystać z kilku poleceń takich jak ```cut``` albo ```awk```. Wyobraźmy sobie, że mamy plik ```example.csv```, który zawiera dane o pracownikach w firmie.

- Wyświetlenie pierwszej kolumny pliku: 
```
cut -d ',' -f 1 example.csv
```
- Wyświetlenie rekordów, gdzie wartość w drugiej kolumnie jest równa "Manager":
```
awk -F ',' '$2 == "Manager"' example.csv
```

## Głębsze pogłębienie

Format CSV został stworzony w 1972 roku przez Petera Critchlowa. Jest to jeden z najprostszych sposobów na ustrukturyzowanie danych w postaci tabelarycznej. Alternatywą dla CSV jest format JSON, który jest bardziej popularny w nowszych aplikacjach webowych. W Bash, obsługa plików CSV jest możliwa dzięki wykorzystaniu poleceń takich jak ```sed``` czy ```awk```. Możemy także wykorzystać narzędzia dedykowane do przetwarzania danych tabelarycznych, takie jak Pandas w języku Python.
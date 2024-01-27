---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV-formatet brukes for å lagre og utveksle data. Programmerere jobber med det fordi det er enkelt, universelt og støttes av de fleste programmer og språk.

## Slik gjør du:
Her er enkle eksempler på hvordan lese og skrive CSV-filer i Python.

### Lese CSV:
```Python
import csv

# Åpne filen og lese innholdet med csv.reader
with open('eksempel.csv', mode='r', encoding='utf-8') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)
```

### Skrive til CSV:
```Python
import csv

# Data som skal skrives til CSV
data = [['Navn', 'Alder', 'By'], ['Ola', 29, 'Oslo'], ['Kari', 35, 'Bergen']]

# Åpne filen og skrive data med csv.writer
with open('eksempel_output.csv', mode='w', encoding='utf-8', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(data)
```

## Dypdykk
CSV står for Comma-Separated Values og har vært i bruk siden 1970-tallet. Alternativer til CSV inkluderer JSON og XML som også er populære dataformat. I Python håndterer `csv`-modulen parsing og skriving, men det finnes også tredjepartsbiblioteker som `pandas` som tilbyr mer avanserte funksjoner.

## Se Også
- Python's offisielle dokumentasjon for CSV-modulen: https://docs.python.org/3/library/csv.html
- Pandas-dokumentasjon for å jobbe med CSV: https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.read_csv.html
- W3Schools Python CSV Reader Tutorial: https://www.w3schools.com/python/python_csv.asp

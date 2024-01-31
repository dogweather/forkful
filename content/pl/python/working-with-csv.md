---
title:                "Praca z plikami CSV"
date:                  2024-01-19
simple_title:         "Praca z plikami CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Praca z plikami CSV (ang. Comma-Separated Values) to obsługa danych tabelarycznych w plikach tekstowych, gdzie wartości są rozdzielane przecinkami. Programiści używają tego formatu ze względu na jego prostotę i uniwersalność – łatwość wymiany danych między różnymi programami i językami programowania.

## Jak to zrobić:
```python
import csv

# Odczyt danych z pliku CSV
with open('dane.csv', newline='', encoding='utf-8') as plik:
    czytacz = csv.reader(plik)
    for wiersz in czytacz:
        print(wiersz)

# Zapis danych do pliku CSV
dane = [['imię', 'nazwisko'], ['Jan', 'Kowalski'], ['Anna', 'Nowak']]
with open('wyniki.csv', mode='w', newline='', encoding='utf-8') as plik:
    pisarz = csv.writer(plik)
    for rekord in dane:
        pisarz.writerow(rekord)
```

## Głębsze spojrzenie:
CSV to format znany od lat 70., gdy po raz pierwszy został użyty w ramach systemu operacyjnego Unix. Alternatywą dla CSV są formaty jak JSON czy XML, które pozwalają na bardziej skomplikowaną strukturę danych. Implementacja obsługi CSV w Pythonie jest prosta dzięki wbudowanemu modułowi `csv`. Warto jednak pamiętać o odpowiednim kodowaniu pliku tekstowego (np. UTF-8), aby uniknąć problemów z polskimi znakami.

## Zobacz również:
- Dokumentacja Pythona dla modułu csv: https://docs.python.org/3/library/csv.html
- Porównanie formatów danych CSV, JSON, XML: https://www.datacamp.com/community/tutorials/csv-json-xml-difference
- Porady dotyczące kodowania UTF-8 w Pythonie: https://docs.python.org/3/howto/unicode.html

---
title:                "Praca z plikami csv"
html_title:           "Python: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV to popularny format plików używany do przechowywania i przetwarzania danych tabelarycznych. Jest bardzo przydatny dla programistów, gdyż umożliwia łatwą wymianę danych między różnymi aplikacjami oraz możliwość ich dalszej obróbki.

## Jak to zrobić

Konfiguracja środowiska:
```Python
import csv
```

Tworzenie pliku CSV:
```Python
with open('dane.csv', 'w', newline='') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(["Imię", "Nazwisko", "Wiek"])
    writer.writerow(["Jan", "Kowalski", 35])
    writer.writerow(["Anna", "Nowak", 28])
```

Odczytywanie pliku CSV:
```Python
with open('dane.csv', 'r') as csv_file:
    reader = csv.reader(csv_file)
    for row in reader:
        print(row)
```

Zapisywanie danych do listy:
```Python
with open('dane.csv', 'r') as csv_file:
    reader = csv.reader(csv_file)
    data = list(reader)
print(data)
```

## Głębsza analiza

CSV (Comma Separated Values) to format plików zwykle używany do przechowywania danych tabelarycznych, takich jak listy lub tabele. Każda linia pliku CSV reprezentuje jeden wiersz danych, a poszczególne pola są oddzielone przecinkami. Jest to format uniwersalny, który może być używany przez różne programy do przetwarzania danych.

Podstawowy format pliku CSV można szybko i łatwo utworzyć lub odczytać za pomocą gotowych modułów jak `csv` w Pythonie. Przykładowe operacje, takie jak zapisywanie danych do listy lub wyświetlanie ich w konsoli, mają prostą i intuicyjną składnię. Jednak przy bardziej zaawansowanym przetwarzaniu danych w pliku CSV, warto zapoznać się z dokumentacją, aby dowiedzieć się o innych dostępnych funkcjach i opcjach.

## Zobacz również

- [Dokumentacja modułu CSV w Pythonie](https://docs.python.org/3/library/csv.html)
- [Poradnik dla początkujących: Obsługa plików CSV w Pythonie](https://realpython.com/python-csv/)
- [10 przydatnych operacji na plikach CSV w Pythonie](https://www.datacamp.com/community/tutorials/pandas-read-csv)
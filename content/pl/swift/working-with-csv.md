---
title:                "Praca z plikami csv"
html_title:           "Swift: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV (ang. Comma Separated Values) jest popularnym formatem przechowywania danych w tabelach i arkuszach kalkulacyjnych. Samoistne przetwarzanie tego formatu może być uciążliwe, dlatego warto poznać narzędzia, które ułatwią jego obsługę.

## Jak to zrobić

```Swift
let csv = """
imie,nazwisko,wiek
Jan,Kowalski,35
Anna,Nowak,28
Piotr,Marciniak,42
"""

let rows = csv
    .components(separatedBy: "\n")
    .map{ $0.components(separatedBy: ",") }

let headers = rows[0] // ["imie", "nazwisko", "wiek"]

for row in rows[1...] {
    let person = Dictionary(uniqueKeysWithValues: zip(headers, row))
    print(person)
}

// ["imie": "Jan", "nazwisko": "Kowalski", "wiek": "35"]
// ["imie": "Anna", "nazwisko": "Nowak", "wiek": "28"]
// ["imie": "Piotr", "nazwisko": "Marciniak", "wiek": "42"]
```

Korzystając z metody `components(separatedBy:)` możemy podzielić nasz plik CSV na wiersze, a następnie użyć `zip` i `Dictionary` aby stworzyć słownik zawierający nagłówki i dane z danego wiersza. W ten sposób możemy łatwo przetwarzać dane z pliku CSV.

## Głębszy zanurzanie się

Istnieje wiele bibliotek i frameworków dostępnych w języku Swift do pracy z plikami CSV. Jednym z nich jest `SwiftCSV`, pozwalający na łatwe wczytywanie i zapisywanie danych w formacie CSV. Istnieją również wiele rozszerzeń do popularnych bibliotek, takich jak `SwiftyJSON`, które umożliwiają prostą obsługę danych w formacie CSV.

## Zobacz również

- https://github.com/yaslab/CSV.swift
- https://github.com/evgenyneu/SwiftCSV
- https://github.com/tid-kijyun/Kanna (biblioteka do parsowania HTML zawierająca funkcje przetwarzania plików CSV)
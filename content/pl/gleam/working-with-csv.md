---
title:                "Gleam: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z plikami CSV jest nieodłączną częścią wielu działań w programowaniu. Może to być konieczne do importowania lub eksportowania danych, tworzenia raportów, czy też analizy statystyk. Gleam daje nam narzędzia, aby łatwo i skutecznie przeprowadzić te procesy, co sprawia, że jest to wartościowym językiem dla każdego programisty.

## Jak to zrobić

Aby pracować z plikami CSV w Gleam, musimy najpierw zaimportować potrzebne biblioteki:

``` Gleam
import csv
import csv/options
```

Następnie możemy otworzyć plik CSV za pomocą funkcji `csv.from_file`, podając jako argument ścieżkę do naszego pliku:

``` Gleam
let csv_file = csv.from_file("nazwa_pliku.csv")
```

Aby odczytać zawartość pliku, możemy użyć funkcji `csv.parse`, która zwraca listę list z danymi z pliku. Przykładowo, jeśli nasz plik CSV zawiera informacje o produktach, możemy je odczytać i wyświetlić w ten sposób:

``` Gleam
let products = csv.parse(csv_file)
// wynik: [[Product Name, Price, Quantity], ["Kubek", "10.99", "50"], ["Koszulka", "29.99", "20"]]

Enum.map(products, fn(product) ->
  let [name, price, quantity] = product
  let output = "Produkt: #{name}, Cena: #{price} PLN, Dostępne sztuk: #{quantity}"
  output
end)
// wynik: [
//  "Produkt: Kubek, Cena: 10.99 PLN, Dostępne sztuk: 50",
//  "Produkt: Koszulka, Cena: 29.99 PLN, Dostępne sztuk: 20"
// ]
```

W ten sam sposób możemy również tworzyć i zapisywać nowe pliki CSV za pomocą funkcji `csv.to_file`.

## Głębszy zanurzenie

Istnieją również inne opcje, którymi możemy manipulować podczas pracy z plikami CSV w Gleam. Na przykład możemy użyć opcji `separator` do określenia separatora używanego w pliku CSV, lub opcji `headers`, aby tworzyć pliki CSV z nagłówkami kolumn.

``` Gleam
let csv_file = csv.from_file("nazwa_pliku.csv", options {separator: ";", headers: ["Kategoria", "Produkt", "Cena"]})
```

Możemy również użyć opcji `charset` do określenia kodowania znaków, oraz opcji `quote_char` do umieszczenia wartości w cudzysłowach.

Pełna dokumentacja biblioteki CSV dla Gleam jest dostępna [tutaj](https://hexdocs.pm/gleam_csv/readme.html). Zachęcamy również do eksperymentowania z różnymi dostępnymi opcjami i poznawania ich funkcjonalności.

## Zobacz też

- [Dokumentacja Gleam CSV](https://hexdocs.pm/gleam_csv/readme.html)
- [Oficjalna strona Gleam](https://gleam.run/)
- [Repozytorium GitHub Gleam](https://github.com/gleam-lang/gleam)
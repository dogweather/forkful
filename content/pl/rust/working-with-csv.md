---
title:                "Praca z plikami csv"
html_title:           "Rust: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV w języku Rust jest procesem polegającym na odczytywaniu lub zapisywaniu danych z plików tekstowych, w których dane są oddzielane przecinkami. Programiści wykonują tę czynność, ponieważ pliki CSV są często wykorzystywane do przechowywania danych tabelarycznych, a praca z nimi jest niezbędna w wielu projektach.

## Jak to zrobić:

```Rust
use csv;

fn main() {
    // Otwieranie pliku CSV do odczytu
    let mut reader = csv::Reader::from_path("plik.csv").unwrap();

    // Pętla do odczytywania danych z każdej kolumny
    for result in reader.records() {
        // Zwraca wartość typu Result z rekordem lub błędem
        let record = result.unwrap();

        // Przykładowe użycie danych z kolumny
        let imie: &str = record.get(0).unwrap();
        let wiek: usize = record.get(1).unwrap();

        println!("{} lat {}", imie, wiek);
    }
}
```

Przykładowy plik CSV "plik.csv":

```
Imie, Wiek
Anna, 25
Jan, 30
```

Wynik:

```
Anna lat 25
Jan lat 30
```

## Głębsze zagadnienia:

- CSV (ang. Comma-Separated Values) jest formatem plików tekstowych, w których dane są oddzielane znakiem przecinka.
- Możliwe jest również zapisanie danych w innych formatach, takich jak JSON czy XML, jednak pliki CSV są często wykorzystywane ze względu na swoją prostotę i czytelność.
- Implementacja biblioteki csv w języku Rust jest oparta na strukturach danych wykorzystujących metody borrow checker i "zera-kości".

## Zobacz także:

- [Dokumentacja dla biblioteki CSV w języku Rust](https://docs.rs/csv/)
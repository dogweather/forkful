---
title:                "Rust: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV (ang. Comma-Separated Values) to popularny format plików używany w celu przechowywania danych tabelarycznych. Dzięki swojej prostocie i wszechstronności, jest często wybieranym przez programistów formatem do przetwarzania i analizy danych. W tym artykule dowiesz się, dlaczego warto nauczyć się pracować z CSV w języku Rust.

## Jak to zrobić

Pierwszym krokiem do pracy z CSV w Rust jest zaimportowanie niezbędnych bibliotek. Na przykład, jeśli korzystasz z Cargo, możesz dodać do pliku `Cargo.toml` następującą linię:

```Rust
csv = "1.0.0"
```

Następnie, w kodzie, musisz zaimplementować odpowiednie struktury danych dla pliku CSV, takie jak nagłówki i wiersze. Możesz to zrobić, tworząc nowy wektor lub strukturę `HashMap` w zależności od preferencji.

```Rust
let mut rdr = csv::Reader::from_path("plik.csv")?;
for result in rdr.records() {
    let record = result?;
    // przetwarzanie danych
}
```

Aby dostępować do konkretnych wartości wiersza, możesz użyć metody `get` lub `unwrap`:

```Rust
let country: Option<&str> = row.get(0);
```

## Głębokie odkrycie

Pracując z CSV w Rust, możesz wykorzystać dostępne funkcje, takie jak sortowanie, filtrowanie i grupowanie danych. Możesz również używać różnych typów danych, takich jak liczby, rodzaje, daty, itp. Korzystając z funkcji `serde`, możesz nawet łatwo konwertować CSV na inne formaty, takie jak JSON czy XML.

Ponadto, dzięki koncepcji "borrowing" w Rust, możesz uniknąć problemów związanych z zarządzaniem pamięcią i wyciekami. Dzięki temu, praca z dużymi plikami CSV jest bardziej wydajna i bezpieczna.

## Zobacz również

Jeśli interesuje Cię bardziej zaawansowana praca z CSV w języku Rust, polecamy zapoznać się z dokumentacją biblioteki `csv` oraz przeczytać artykuł "Effective Rust: working with CSV files" na stronie [Dev.to](https://dev.to/infosiftr/rust-csv) oraz naukę na platformie [Rustlang](https://www.rust-lang.org/learn). Oba materiały są dostępne w języku polskim i pomogą Ci lepiej poznać możliwości tego języka w pracy z formatem CSV.
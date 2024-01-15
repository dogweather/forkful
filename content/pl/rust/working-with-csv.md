---
title:                "Praca z csv"
html_title:           "Rust: Praca z csv"
simple_title:         "Praca z csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto pracować z formatem CSV? To jedno z najpopularniejszych formatów do przechowywania danych w plikach tekstowych. Dzięki swojej prostocie i wszechstronności, jest wykorzystywany w wielu dziedzinach, takich jak finanse, nauka czy handel.

## Jak to zrobić

Rust to język programowania, który jest idealny do pracy z CSV. Dzięki swojej wydajności i bezpieczeństwu, jest coraz częściej wybierany przez programistów. Oto przykładowy kod, który wczyta plik CSV o nazwie "dane.csv" i wyświetli jego zawartość w konsoli:

```Rust
use std::fs::File;
use std::io::{BufRead, BufReader};
use csv::Reader;

fn main() {
    let file = File::open("dane.csv").unwrap();
    let reader = BufReader::new(file);
    let mut csv_reader = Reader::from_reader(reader);

    for result in csv_reader.records() {
        let record = result.unwrap();
        println!("{:?}", record);
    }
}
```

Output:
```Rust
["Imię", "Nazwisko", "Wiek"]
["Jan", "Kowalski", "30"]
["Anna", "Nowak", "25"]
```

Możemy również manipulować danymi i wykorzystać je w dalszej obróbce w naszym programie:

```Rust
use std::fs::File;
use std::io::{BufRead, BufReader};
use csv::Reader;

fn main() {
    let file = File::open("dane.csv").unwrap();
    let reader = BufReader::new(file);
    let mut csv_reader = Reader::from_reader(reader);
    
    let mut people = Vec::new(); //tworzymy pustą wektor do przechowywania danych

    for result in csv_reader.records() {
        let record = result.unwrap();
        let name = record[0].to_string(); //zapisujemy imię do zmiennej
        let age:u32 = record[2].parse().unwrap(); //konwertujemy wiek na liczbę
        
        people.push((name, age)); //dodajemy dane do wektora
    }

    println!("{:?}", people);
}
```

Output:
```Rust
[("Jan", 30), ("Anna", 25)]
```

## Głębsza analiza

Rust oferuje wiele narzędzi do pracy z plikami CSV, takich jak biblioteki csv, csv-core czy csv-writer. Możemy również wykorzystać makra, aby uprościć nasze zadanie. Warto również pamiętać o przetwarzaniu błędów, aby nasz kod był bezpieczny i nie występowały nieoczekiwane problemy.

## Zobacz także

- Dokumentacja Rust dla pracy z plikami CSV: https://docs.rs/csv
- Przykładowe projekty wykorzystujące Rust do pracy z CSV: https://github.com/rust-csv/
- Poradnik na temat parsowania plików CSV w języku Rust: https://blog.logrocket.com/working-with-csv-in-rust/
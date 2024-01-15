---
title:                "Praca z json"
html_title:           "Rust: Praca z json"
simple_title:         "Praca z json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Sporządzanie i odczytywanie danych w formacie JSON jest ważnym elementem programowania. React, Angular i inne technologie webowe wykorzystują JSON do przesyłania danych między front-endem a serwerem. Dlatego warto nauczyć się pracy z tym formatem danych.

## Jak to zrobić

Sama praca z JSON w języku Rust jest bardzo prosta. Wystarczy użyć biblioteki `serde` i jej makra, aby łatwo serializować i deserializować dane. Oto kilka przykładów kodu:

```Rust
// importowanie biblioteki serde
extern crate serde;
use serde::{Serialize, Deserialize};

// struktura danych do serializacji
#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    address: Address,
}

// przykładowe dane do serializacji
let person = Person {
    name: "John".to_string(),
    age: 25,
    address: Address {
        city: "New York".to_string(),
        country: "USA".to_string(),
    }
};

// serializacja do formatu JSON
let json = serde_json::to_string(&person).unwrap();
println!("{}", json); // {"name":"John","age":25,"address":{"city":"New York","country":"USA"}}

// deserializacja z formatu JSON
let new_person: Person = serde_json::from_str(&json).unwrap();
println!("{}, {}, {}", new_person.name, new_person.age, new_person.address.city); // John, 25, New York 
```

Powyższy kod pokazuje jak łatwo można serializować i deserializować struktury danych do i z formatu JSON. W przypadku bardziej złożonych danych, można łatwo użyć makra `serde` do określenia niestandardowej struktury danych. 

## Vertigo Dive

Jeśli interesuje Cię bardziej szczegółowe informacje na temat pracy z JSON w języku Rust, to polecam odwiedzić stronę [dokumentacji biblioteki serde](https://serde.rs/index.html). Znajdziesz tam dokładne wyjaśnienia działania makr i przykłady z bardziej skomplikowanymi strukturami danych. 

## Zobacz także

- [https://doc.rust-lang.org/stable/book/second-edition/ch16-00-concurrency.html#working-with-json](https://doc.rust-lang.org/stable/book/second-edition/ch16-00-concurrency.html#working-with-json)
- [https://serde.rs/index.html](https://serde.rs/index.html)
- [https://github.com/serde-rs/json](https://github.com/serde-rs/json)
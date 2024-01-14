---
title:                "Rust: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON jest jednym z najpopularniejszych formatów danych używanych w dzisiejszych aplikacjach internetowych. Jest on prosty w użyciu, czytelny przez człowieka i łatwy do przetwarzania przez komputer. W tym artykule dowiesz się dlaczego warto poznać język Rust i jak go używać do pracy z formatem JSON.

## Jak to zrobić

Aby pracować z JSON w języku Rust, musisz najpierw zaimportować odpowiednią bibliotekę. W poniższym przykładzie użyjemy popularnej biblioteki serde_json. Następnie możesz użyć metody `to_string` aby przekonwertować obiekt na format JSON.

```Rust
use serde_json::{Value, from_str, to_string};

// Konwertowanie obiektu na JSON
let obj = json!({
    "imie": "Anna",
    "wiek": 25,
    "kolor_oczu": "zielony"
});

let json_string = to_string(&obj).unwrap();
println!("{}", json_string); // {"imie": "Anna", "wiek": 25, "kolor_oczu": "zielony"}

// Konwertowanie JSON na obiekt
let data = r#"
    {
        "imie": "Piotr",
        "wiek": 30,
        "kolor_oczu": "niebieski"
    }
"#;

let parsed: Value = from_str(data).unwrap();
let name = parsed["imie"].as_str().unwrap();
let age = parsed["wiek"].as_i64().unwrap();
let eye_color = parsed["kolor_oczu"].as_str().unwrap();
println!("{} ma {} lat i ma {} oczy.", name, age, eye_color); // Piotr ma 30 lat i ma niebieskie oczy.
```

## Wchodzenie w głębię tematu

W języku Rust istnieje wiele bibliotek pozwalających na pracę z formatem JSON. Każda z nich ma swoje zalety i wady. Możesz również przeglądać dokumentację bibliotek, aby dowiedzieć się więcej o dostępnych metodach i sposobach przetwarzania danych.

Niektóre z przydatnych funkcji dotyczących pracy z JSON w języku Rust to:

- Tworzenie obiektów i tablic z danymi w formacie JSON za pomocą makra `json!`
- Automatyczne serializowanie i deserializowanie obiektów przy użyciu metody `serde`
- Walidacja i konwersja danych przy użyciu metody `serde_json`

Dzięki językowi Rust i jego wszechstronnym bibliotekom, praca z formatem JSON stała się jeszcze łatwiejsza i wydajniejsza.

## Zobacz także

- Dokumentacja biblioteki serde_json: https://docs.rs/serde_json/
- Przykłady kodu: https://github.com/serde-rs/json
- Oficjalna strona języka Rust: https://www.rust-lang.org/
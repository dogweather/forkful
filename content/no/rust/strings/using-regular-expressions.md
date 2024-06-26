---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:22.327591-07:00
description: "Hvordan: Rusts `regex`-bibliotek er det man g\xE5r til for \xE5 jobbe\
  \ med regul\xE6re uttrykk. For \xE5 bruke det, m\xE5 du f\xF8rst legge det til i\
  \ din `Cargo.toml`."
lastmod: '2024-03-13T22:44:40.564038-06:00'
model: gpt-4-0125-preview
summary: "Rusts `regex`-bibliotek er det man g\xE5r til for \xE5 jobbe med regul\xE6\
  re uttrykk."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hvordan:
Rusts `regex`-bibliotek er det man går til for å jobbe med regulære uttrykk. For å bruke det, må du først legge det til i din `Cargo.toml`:

```toml
[dependencies]
regex = "1"
```

Deretter kan du starte implementeringen av regex-funksjonaliteter i Rust-koden din. Her er hvordan du utfører noen vanlige operasjoner:

### Å Finne et Mønster i en Streng
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let dato = "2023-04-15";

    println!("Samsvarer teksten med datomønsteret? {}", re.is_match(dato));
    // Output: Samsvarer teksten med datomønsteret? true
}
```

### Å Finne og Få Tilgang til Treff
```rust
use regex::Regex;

fn main() {
    let tekst = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(tekst) {
        println!("Språk: {}, År: {}", &cap[1], &cap[2]);
    }
    // Output:
    // Språk: Rust, År: 2023
    // Språk: C++, År: 2022
    // Språk: Python, År: 2021
}
```

### Å Erstatte Tekst
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let tekst = "Rust 2023, C++ 2022, Python 2021";
    let erstattet = re.replace_all(tekst, "$1 ble oppdatert i $2");

    println!("Oppdatert tekst: {}", erstattet);
    // Output: Oppdatert tekst: Rust ble oppdatert i 2023, C++ ble oppdatert i 2022, Python ble oppdatert i 2021
}
```

### Å Dele Tekst ved Bruk av en Regex
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // del ved ethvert ikke-ord-tegn
    let tekst = "Rust-C++-Python-Go";

    let felt: Vec<&str> = re.split(tekst).collect();

    for field in felt {
        println!("Språk: {}", field);
    }
    // Output:
    // Språk: Rust
    // Språk: C++
    // Språk: Python
    // Språk: Go
}
```

Disse eksemplene gir en grunnleggende veiledning for å komme i gang med regulære uttrykk i Rust. Etter hvert som dine behov blir mer sofistikerte, tilbyr `regex`-kassen en mengde funksjonalitet for komplekse mønstersøknings- og tekstmanipuleringsoppgaver.

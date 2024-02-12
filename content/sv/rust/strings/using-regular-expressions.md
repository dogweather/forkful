---
title:                "Att använda reguljära uttryck"
aliases:
- /sv/rust/using-regular-expressions.md
date:                  2024-02-03T19:18:27.531579-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda reguljära uttryck"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad och varför?

Reguljära uttryck, eller regex, låter utvecklare söka, matcha och manipulera strängar med avancerade mönsterpassningstekniker. I Rust hjälper användning av regex till att effektivt tolka och hantera textdata, vilket gör uppgifter som datavalidering, sökning och texttransformationer mer strömlinjeformade och underhållbara.

## Hur man gör:

Rusts `regex`-bibliotek är det givna valet för att arbeta med reguljära uttryck. För att använda det måste du först lägga till det i din `Cargo.toml`:

```toml
[dependencies]
regex = "1"
```

Sedan kan du börja implementera regex-funktionaliteter i din Rust-kod. Så här utför du några vanliga operationer:

### Matcha ett mönster i en sträng

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let datum = "2023-04-15";

    println!("Stämmer texten överens med datumsmönstret? {}", re.is_match(datum));
    // Utdata: Stämmer texten överens med datumsmönstret? true
}
```

### Hitta och komma åt matchningar

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("Språk: {}, År: {}", &cap[1], &cap[2]);
    }
    // Utdata:
    // Språk: Rust, År: 2023
    // Språk: C++, År: 2022
    // Språk: Python, År: 2021
}
```

### Ersätta text

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let ersatt = re.replace_all(text, "$1 uppdaterades år $2");

    println!("Uppdaterad text: {}", ersatt);
    // Utdata: Uppdaterad text: Rust uppdaterades år 2023, C++ uppdaterades år 2022, Python uppdaterades år 2021
}
```

### Dela upp text med hjälp av ett regex

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // dela vid varje tecken som inte är ett ord
    let text = "Rust-C++-Python-Go";

    let fält: Vec<&str> = re.split(text).collect();

    for fält in fält {
        println!("Språk: {}", fält);
    }
    // Utdata:
    // Språk: Rust
    // Språk: C++
    // Språk: Python
    // Språk: Go
}
```

Dessa exempel ger en grundläggande guide för att komma igång med reguljära uttryck i Rust. När dina behov blir mer sofistikerade erbjuder `regex`-craten en mängd funktionalitet för komplex mönsterpassning och textmanipuleringsuppgifter.

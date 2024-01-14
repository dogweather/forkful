---
title:                "Rust: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne blogginnlegget skal vi utforske en viktig del av Rust-programmering: å concatenere, eller sette sammen, strings. Dette er en vanlig oppgave i mange programmer og kan være spesielt nyttig når du ønsker å sette sammen tekststrenger for å presentere informasjon eller lage dynamiske meldinger. Fortsett å lese for å lære mer om hvorfor og hvordan du kan gjøre dette i Rust!

## Hvordan

Det første du må gjøre er å inkludere `std::fmt` biblioteket i Rust-programmet ditt. Dette biblioteket gir deg funksjoner for å formatere og skrive ut tekststrenger. Deretter kan du bruke `.to_string()` metoden for å konvertere verdier til strings. Se et eksempel nedenfor:

```rust
// Importer `std::fmt` biblioteket
use std::fmt;

// Definer en struct med to verdier
struct Person {
    navn: String,
    alder: u8,
}

// Implementer `fmt::Display` for structen vår
impl fmt::Display for Person {
    // `fmt` tar imot en referanse til `self` og en formatter
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Bruk metoden `write!` for å sette sammen strings
        write!(f, "Hei, jeg heter {} og er {} år gammel.", self.navn, self.alder)
    }
}

fn main() {
    let John = Person { navn: String::from("John"), alder: 25 };

    // Print ut teksten
    println!("{}", John.to_string());
}
```

Dette vil gi følgende output:

```
Hei, jeg heter John og er 25 år gammel.
```

## Dykk dypere

Nå som vi har et grunnleggende eksempel, la oss se på noen flere funksjoner og metoder som kan være nyttige når du skal concatenere strings i Rust.

### `format!` Makroen

I tillegg til `write!` metoden, kan du også bruke `format!` makroen for å sette sammen strings. Se på eksempelet nedenfor:

```rust
// Importer `std::fmt` biblioteket
use std::fmt;

// Definer en `Printable` trait
trait Printable {
    fn print(&self) -> String;
}

// Implementer `Printable` trait for `i32` type
impl Printable for i32 {
    fn print(&self) -> String {
        self.to_string()
    }
}

fn main() {
    let num = 42;

    // Sett sammen to strings
    let result = format!("Svaret på alt er: {}", num.print());

    // Print ut resultatet
    println!("{}", result);
}
```

Dette vil gi følgende output:

```
Svaret på alt er: 42
```

### Bruke `+=` eller `push_str()`

Du kan også bruke `+=` operatoren eller `push_str()` metoden for å concatenere strings i Rust. Se et eksempel nedenfor:

```rust
fn main() {
    let mut s = String::from("Hello ");

    // Bruk `+=` operatoren eller `push_str()` metoden for å concatenere en string
    s += "World!";
    s.push_str(" I'm Rust!");

    // Print ut resultatet
    println!("{}", s);
}
```

Dette vil gi følgende output:

```
Hello World! I'm Rust!
```

## Se også

Nå som du forstår grunnleggende om hvordan du kan concatenate strings i Rust, kan du prøve å utforske mer av `std::fmt` biblioteket for flere nyttige funksjoner. Du kan også se på offisiell dokumentasjon for mer informasjon og eksempler:

- [Offisiell Rust dokumentasjon om `std::fmt`](https://doc.rust-lang.org/std/fmt/index.html)
- [Rust By Example sin seksjon om `std::fmt`](https://doc.rust-lang.org/rust-by-example/fn.html)

Lykke til med å sette sammen strings i dine Rust-programmer!
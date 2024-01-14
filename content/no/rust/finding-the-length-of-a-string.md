---
title:    "Rust: Å finne lengden på en streng"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer som er interessert i effektivitet og lavere minneforbruk, vil du kanskje lurer på hvorfor det er viktig å finne lengden av en streng i Rust. Å kunne finne lengden av en streng på en effektiv måte er en viktig ferdighet i Rust-programmering, spesielt når du håndterer store mengder data.

## Slik gjør du det

For å finne lengden av en streng i Rust, kan du bruke funksjonen `len()` fra `std::str` biblioteket. Her er et eksempel på hvordan dette kan gjøres:

```
use std::str;

fn main() {
  let my_string = "Hei, verden!";
  let length = len(my_string);
  println!("Lengden av strengen er: {}", length);
}
```

Dette vil gi følgende utskrift:

```
Lengden av strengen er: 12
```

Du kan også bruke `.len()` metoden på en streng for å finne lengden:

```
fn main() {
  let my_string = "Hei, verden!";
  let length = my_string.len();
  println!("Lengden av strengen er: {}", length);
}
```

Dette gir samme resultat som eksempelet ovenfor.

## Utforske videre

Å finne lengden av en streng i Rust kan virke enkelt, men det er noen ting som er verdt å utforske videre. For eksempel, hvordan fungerer `len()` funksjonen egentlig og hvordan håndterer Rust forskjellige typer strenger? Du kan også se nærmere på ytelsesforskjeller mellom å bruke `.len()` metoden og `len()` funksjonen.

## Se også

- [Rust Documentation: std::str](https://doc.rust-lang.org/std/str/index.html)
- [Rust by Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Strings in Rust: A Deep Dive](https://blog.logrocket.com/strings-in-rust-a-deep-dive/)
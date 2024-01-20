---
title:                "Starter et nytt prosjekt"
html_title:           "Arduino: Starter et nytt prosjekt"
simple_title:         "Starter et nytt prosjekt"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?
Å starte et nytt prosjekt er å lage en nytt kodebase for et bestemt formål. Programmerere gjør dette for å begynne å løse en annen problemstilling eller utvikle en ny programvare.

## Slik gjør du:
Her er et eksempel på hvordan man kan starte med et nytt prosjekt i Rust med ```cargo init```.

```Rust
$ cargo new mitt_prosjekt
$ cd mitt_prosjekt
```

Dette vil generere en ny katalog kalt `mitt_prosjekt` med en enkel "Hello, World!" kodefil og en Cargo.toml fil, din pakkemanager for Rust prosjekt.

Koden vil se slik ut:

```Rust
fn main() {
    println!("Hei, Verden!");
}
```

Kjør prosjektet ditt med ```cargo run``` i din terminal.

```Rust
$ cargo run
   Compiling mitt_prosjekt v0.1.0 (/path/to/your/project/mitt_prosjekt)
    Finished dev [unoptimized + debuginfo] target(s) in 2.83s
     Running `target/debug/mitt_prosjekt`
Hei, Verden!
```

Som du ser, sier programmet "Hei, Verden!".

## Dypdykk
Rust ble først utgitt i 2010 av Mozilla Research for å løse problemer med system programmering. De ønsket et språk som ga kontroll over systemressurser, men som også vektla hastighet og sikkerhet.

Alternativt kan man kan også bruke andre språk som Go, C++, Java for system programmering, men Rust har fått mye oppmerksomhet på grunn av sin ytelse og memory-safe design.

Når det gjelder Rust prosjekter, er det viktig å merke seg at Cargo spiller en vesentlig rolle i prosjektadministrasjon. Det håndterer Rust pakker (kalt "crates") og sikrer at riktig versjon er installert og kompilert.

## Se Også
For mer i dybden informasjon, se følgende ressurser:
1. [The Rust Programming Language book](https://doc.rust-lang.org/book/)
2. [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/)
3. [Rust-Lang website](https://www.rust-lang.org/)
4. [Cargo Guide](https://doc.rust-lang.org/cargo/guide/)
---
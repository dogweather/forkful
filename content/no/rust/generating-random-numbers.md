---
title:    "Rust: Generering av tilfeldige tall"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang hatt behov for å generere tilfeldige tall i ditt Rust-program, men ikke visst hvordan du skal gjøre det? Da er du ikke alene. Generering av tilfeldige tall er et vanlig behov i mange programmeringsprosjekter, og i denne bloggposten vil jeg vise deg hvordan du kan gjøre det på en enkel måte ved hjelp av Rust.

## Hvordan gjøre det

Det finnes flere måter å generere tilfeldige tall på i Rust, men den enkleste metoden er å bruke standardbiblioteket til Rust, std::rand. For å bruke dette biblioteket må du først legge til følgende linje i din *Cargo.toml* fil:

```rust
[dependencies]
rand = "0.8"
```

Når dette er gjort, kan du generere tilfeldige tall ved hjelp av følgende eksempelkode:

```rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let random_number: u32 = rng.gen_range(1..=100);
    println!("Det tilfeldige tallet er: {}", random_number);
}
```

La oss bryte ned koden og se hva den gjør. Først importerer vi *Rng* fra *rand* biblioteket, som lar oss bruke metoder for å generere tilfeldige tall. Deretter definerer vi en *mut* variabel som heter *rng*, som representerer en "random number generator" i Rust. Vi definerer også en *u32* variabel ved navn *random_number*, og ved hjelp av metoden *gen_range* fra *rng* variabelen, gir vi den en range, i dette tilfellet fra 1 til og med 100. Til slutt printer vi ut det tilfeldige tallet ved hjelp av *println!* makroen.

Når du kjører denne koden, vil du få et tilfeldig tall mellom 1 og 100 hver gang du kjører den.

## Dypdykk

Hittil har vi bare sett på hvordan vi kan generere et enkelt tilfeldig tall, men hva om vi trenger å generere flere tall eller til og med en liste med tilfeldige tall? Det finnes flere metoder for å gjøre dette, men en enkel og effektiv metode er å bruke en "*for loop*" og lagre tallene i en vektor. Se eksempelkoden nedenfor:

```rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let mut random_numbers: Vec<u32> = Vec::new();

    for _ in 0..5 {
        let random_number: u32 = rng.gen_range(1..=100);
        random_numbers.push(random_number);
    }

    println!("De tilfeldige tallene er: {:?}", random_numbers);
}
```

Her har vi definert et heltall som heter *random_numbers*, som vil inneholde våre tilfeldige tall. Inne i "*for loop*" kan vi bruke metoden *push* for å legge til det tilfeldige tallet vi genererer i hver iterasjon i vektoren *random_numbers*. Til slutt printer vi ut alle tallene ved hjelp av *println!* makroen.

## Se også

- [Rust crates for random number generation](https://rust-random.github.io/book/guide-rngs.html)
- [Official Rust documentation for std::rand](https://doc.rust-lang.org/stable/std/rand/)
- [Generating random numbers in Rust with the rand crate](https://rustbyexample.com/std/rand.html)
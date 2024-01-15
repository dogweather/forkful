---
title:                "Generering av tilfeldige tall"
html_title:           "Rust: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville du trenge tilfeldig genererte tall i koden din? Vel, det kan være flere grunner til dette. Kanskje du trenger å lage et spill eller en simulering som krever tilfeldigheter for å gjøre det mer realistisk. Eller kanskje du vil legge til en element av tilfeldighet i din algoritme for å få mer varierte resultater. Uansett årsak, å generere tilfeldige tall er en viktig del av mange programmer.

## Slik gjør du det

For å generere tilfeldige tall i Rust, må du importere "rand" biblioteket. Deretter kan du opprette en ny instans av "ThreadRng" type ved hjelp av "rand::thread_rng()" funksjonen. Dette gir deg tilgang til en tilfeldighetsgenerator som kan brukes til å generere ulike typer tall. Her er noen eksempler:

```Rust
// Genererer et tilfeldig heltall mellom 1 og 10
let num = rand::thread_rng().gen_range(1..11);
println!("Tallet mitt er: {}", num);

// Genererer et tilfeldig desimaltall mellom 0 og 1
let dec_num = rand::thread_rng().gen_range(0.0..1.0);
println!("Tallet mitt er: {}", dec_num);

// Genererer et tilfeldig boolsk verdi
let boolean = rand::thread_rng().gen_bool(0.5);
println!("Boolean verdi: {}", boolean);
```

Dette er bare noen få eksempler på hvordan du kan bruke tilfeldighetsgeneratoren. Sjekk ut dokumentasjonen for "rand" biblioteket for å lære om flere muligheter.

## Dypdykk

Du lurer kanskje på hvordan "rand" biblioteket faktisk genererer tilfeldige tall. Vel, det bruker en pseudorandom generator som er basert på en algoritme kalt "Xorshift". Algoritmen starter med en startverdi og bruker enkle beregninger for å generere en tilsynelatende tilfeldig sekvens av tall. Men, siden dette er en algoritme og ikke helt random, kan sekvensen faktisk gjenta seg selv. Derfor er det viktig å bruke en god startverdi og å ikke bruke algoritmen flere ganger i samme program for å unngå å få de samme tallene.

## Se også

- [Rust Dokumentasjon for "rand" biblioteket](https://docs.rs/rand)
- [En artikel om Xorshift algoritmen](https://www.jstatsoft.org/article/view/v008i14)
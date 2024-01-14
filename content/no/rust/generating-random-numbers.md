---
title:                "Rust: Å generere tilfeldige tall"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hva som skjer når du klikker på en knapp og et tilfeldig tall dukker opp? Eller kanskje du lurer på hvordan dataprogrammer genererer tilfeldige tall? Uansett hva som måtte være grunnen din for å lære å generere tilfeldige tall, så er du på rett sted! I denne bloggposten vil vi lære deg hvordan du kan generere tilfeldige tall ved hjelp av programmeringsspråket Rust. Det kan være nyttig i en rekke forskjellige situasjoner, enten det er i spillutvikling, simuleringer eller andre programmeringsprosjekter.

## Hvordan

```rust
fn main() {
    // Importerer biblioteket "rand" som hjelper oss med å generere tilfeldige tall
    use rand::Rng;

    // Oppretter en variabel som vil inneholde det tilfeldige tallet
    let mut rng = rand::thread_rng();

    // Genererer et tilfeldig tall mellom 1 og 10
    let random_number = rng.gen_range(1, 11);

    // Skriver ut det genererte tallet
    println!("Det tilfeldige tallet er: {}", random_number);
}
```

Kodeeksempelet ovenfor viser hvordan du med hjelp av Rust kan generere et tilfeldig tall mellom 1 og 10. Først importerer vi biblioteket "rand" som inneholder funksjoner og metoder for å generere tilfeldige tall. Deretter oppretter vi en variabel "rng" som vil brukes til å kalle på disse funksjonene. Vi benytter deretter "gen_range" funksjonen for å generere et tall mellom 1 og 10, og deretter skriver vi det ut.

Du kan også bruke andre funksjoner som "gen_bool" for å generere tilfeldige boolske verdier eller "gen_range_f64" for å generere desimaltall. Det finnes også andre måter å tilpasse og kontrollere genereringen av tilfeldige tall på, som for eksempel å spesifisere et seed eller å bruke en annen RNG (Random Number Generator). Utforsk gjerne dokumentasjonen til biblioteket "rand" for mer avanserte eksempler og muligheter.

## Deep Dive

Mens det å generere tilfeldige tall kan virke enkelt i utgangspunktet, er det faktisk en mer kompleks prosess enn man kanskje skulle tro. Det involverer blant annet bruk av matematiske algoritmer, som er designet for å produsere tall som oppfører seg som tilfeldige tall. Dette er viktig for å sikre tilfeldigheten og fordelingen av tallene som blir generert.

Noen av de vanligste algoritmene brukt for å generere tilfeldige tall inkluderer "Linear Congruential Generator", "Mersenne Twister" og "Xorshift". Disse algoritmene har alle ulike egenskaper og fordeler, og det er derfor biblioteket "rand" i Rust bruker en kombinasjon av disse for å generere optimale tilfeldige tall.

Det er også verdt å nevne at tilfeldige tall generert på en datamaskin faktisk ikke er 100% tilfeldige, da de er basert på en algoritme som i utgangspunktet er deterministisk. Det betyr at det samme seedet og de samme algoritmene vil føre til de samme tallene hver gang programmet kjøres. Dette er grunnen til at man vanligvis bruker et seed basert på tiden eller andre variabler for å skape en ilusjon av tilfeldighet.

## Se også

- [Rust dokumentasjon for tilfeldige tall](https://doc.rust-lang.org/stable/rust-by-example/rand.html)
- [Offisiell dokumentasjon for biblioteket "rand"](https://docs.rs/rand/0.8.4/rand/)
- [Sammenligning av ulike RNG-algoritmer](https://www.pcg-random.org/pdf/hmc-cs-2014-0905.pdf)
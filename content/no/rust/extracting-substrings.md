---
title:    "Rust: Utvinning av delstrenger"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Når du programmerer i Rust, er det ofte nyttig å kunne trekke ut deler av en streng som du arbeider med. Dette kan være for å manipulere data eller for å få tilgang til spesifikke deler av informasjonen. I denne bloggposten skal vi se nærmere på hvordan du kan gjøre dette i Rust, og hvorfor det er en verdifull ferdighet å ha.

## Hvordan

For å trekke ut substrings i Rust, bruker vi funksjonen `substring` sammen med indekser. La oss si at vi har en streng som inneholder en setning, og vi bare ønsker å få tak i den første halvdelen av denne setningen. Her er en enkel kodeblokk som demonstrerer hvordan du kan gjøre dette:

```Rust
let sentence = "Dette er en setning.";
let first_half = string[0..10];
println!("{}", first_half);
```

Outputen av dette vil være "Dette er en", som er den første halvdelen av setningen. La oss nå si at vi bare vil ha de tre første ordene. Da kan vi bruke en annen funksjon, `split_whitespace`, til å splitte strengen ved hvert mellomrom. Her er en kodeblokk som viser hvordan dette kan gjøres:

```Rust
let sentence = "Dette er en setning.";
let words: Vec<&str> = sentence.split_whitespace().collect();
let first_three = words[0..3].join(" ");
println!("{}", first_three);
```

Outputen av dette vil være "Dette er en", som er de tre første ordene i setningen. Som du kan se, kan vi kombinere forskjellige funksjoner for å hente ut ulike deler av en streng.

## Deep Dive

En viktig ting å være oppmerksom på når du jobber med substrings i Rust, er at de har en referanse til den opprinnelige strengen. Dette betyr at hvis du gjør endringer på substringen, vil det påvirke den opprinnelige strengen også. Dette kan være nyttig i visse tilfeller, men det kan også føre til uventet oppførsel hvis du ikke er klar over det.

En annen ting å merke seg er at indekser i Rust starter på 0, slik at den første bokstaven i en streng vil ha indeks 0, den andre bokstaven 1, og så videre. Når du bruker en range for å trekke ut substrings, må du huske på at den siste indeksen ikke er inkludert. Dette betyr at hvis du bruker `[0..3]`, vil det være fra indeks 0 til 2, og ikke 3.

## Se også

Her er noen nyttige ressurser for å lære mer om å trekke ut substrings i Rust:

- [Offisiell Rust dokumentasjon](https://doc.rust-lang.org/std/primitive.str.html#method.substring)
- [Rust Programming Language Book](https://doc.rust-lang.org/book/ch04-03-slices.html#working-with-slices-asdfasdf)
- [Rust by Example - Slices](https://doc.rust-lang.org/stable/rust-by-example/slice.html)
- [Forskjellen mellom &str og String i Rust](https://stackoverflow.com/questions/24158114/differences-between-str-and-string/24158297)

Takk for at du leste denne bloggposten om å trekke ut substrings i Rust. Vi håper det har vært nyttig for deg!
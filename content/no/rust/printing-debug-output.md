---
title:    "Rust: Utskrift av feilsøkingsutdata"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Det å printe ut debug output kan være en nyttig måte å feilsøke programmer på. Ved å få ut data fra programmet mens det kjører, kan man enklere identifisere feil og finne løsninger på dem.

## Hvordan

For å printe ut debug output i Rust, kan man bruke makroen ```dbg!()```. Denne makroen tar inn et uttrykk og printer ut både uttrykket og verdien som blir evaluert.

```Rust
let num = 42;
let name = "John";
dbg!(num);
dbg!(name);
```

Dette vil gi følgende output:

```Rust
[src/main.rs:4] num = 42
[src/main.rs:5] name = "John"
```

Man kan også bruke ```dbg!()``` i kombinasjon med en funksjon for å få ut mer kompleks output:

```Rust
fn add_two(num: i32) -> i32 {
    num + 2
}
let num = 42;
dbg!(add_two(num));
```

Output vil da se slik ut:

```Rust
[src/main.rs:5] add_two(num) = 44
```

## Deep Dive

Når man bruker ```dbg!()```-makroen, så blir outputen laget som en stukturert tekststreng som inneholder filnavn, linjenummer og verdien til uttrykket. Dette er til stor hjelp når man feilsøker, spesielt hvis man har flere uttrykk som skal printes ut.

Man kan også bruke ```dbg!()```-makroen på flere uttrykk samtidig, ved å separere dem med komma:

```Rust
let num = 42;
let name = "John";
dbg!(num, name);
```

Output vil da se slik ut:

```Rust
[src/main.rs:4] num = 42, [src/main.rs:5] name = "John"
```

Å printe ut debug output kan også være nyttig når man jobber med lånte verdier eller livetidproblemer i Rust. Man kan da få oversikt over verdier og livetider som kan gi nyttig informasjon for å løse problemet.

## Se også

- [Rust dokumentasjon om ```dbg!()```-makroen](https://doc.rust-lang.org/std/macro.dbg.html)
- [The Rust Programming Language bok, kapittel 10 om debugging](https://doc.rust-lang.org/book/ch10-04-debugging.html) 
- [Coding in Rust, video om debugging med ```dbg!()```-makroen](https://www.youtube.com/watch?v=QKYLo8m42oQ)
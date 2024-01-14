---
title:    "Rust: Utskrift av feilsøkingsutdata"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive ut debug utdata er en viktig del av programvareutvikling. Det lar deg spore og fikse feil i koden din, og gir deg en bedre forståelse av hva som skjer i programmet mens det kjører. I denne bloggposten vil vi utforske hvordan du kan bruke debug utdata i Rust for å forbedre utviklingsprosessen din.

## Hvordan gjøre det

For å skrive ut debug utdata i Rust, kan du bruke makroen `println!` eller `eprintln!` avhengig av om du vil skrive ut til standardutdata eller feilutdata. Her er et eksempel på hvordan du kan skrive ut en variabel i Rust:

```Rust
let navn = "Per";
println!("Hei, mitt navn er {}", navn); // output: Hei, mitt navn er Per 
```

Du kan også legge til flere variabler i utdataen ved å bruke flere plassholdere og liste opp variablene etter utdatastrengen. Det er også mulig å bruke Rusts Debug trait for å få en mer detaljert utskrift av variabler.

```Rust 
let alder = 30;
println!("Jeg er {} år gammel, {:?} måneder", alder, alder); // output: Jeg er 30 år gammel, 30 måneder
```

## Dypdykk

Mens `println!` og `eprintln!` er gode verktøy for å skrive ut enkle utdata, kan du også bruke makroen `dbg!` for å få mer informasjon om variabler og deres verdier. Denne makroen vil skrive ut variabelnavn, verdi og datatype.

```Rust
let liste = vec![1, 2, 3];
println!("{:?}", liste); // output: [1, 2, 3]
dbg!(liste); // output: [src/main.rs:2] liste = [1, 2, 3]
```

Du kan også bruke `dbg!` til å evaluere uttrykk og skrive ut resultatet.

```Rust
let a = 2;
let b = 3;
dbg!(a * b); // output: [src/main.rs:5] a * b = 6
```

## Se også

- Offisiell Rust dokumentasjon for `println!`: https://doc.rust-lang.org/std/macro.println.html
- Rust by Example: https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html
- YouTube tutorial om debugging i Rust: https://www.youtube.com/watch?v=0zm9TQnf_NA
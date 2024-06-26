---
date: 2024-01-27 20:35:25.415544-07:00
description: "Hur man g\xF6r: Rust f\xF6rlitar sig p\xE5 externa crates f\xF6r generering\
  \ av slumpm\xE4ssiga nummer, d\xE4r `rand` \xE4r det mest anv\xE4nda. F\xF6r att\
  \ b\xF6rja generera\u2026"
lastmod: '2024-03-13T22:44:37.693886-06:00'
model: gpt-4-0125-preview
summary: "Rust f\xF6rlitar sig p\xE5 externa crates f\xF6r generering av slumpm\xE4\
  ssiga nummer, d\xE4r `rand` \xE4r det mest anv\xE4nda."
title: Generera slumptal
weight: 12
---

## Hur man gör:
Rust förlitar sig på externa crates för generering av slumpmässiga nummer, där `rand` är det mest använda. För att börja generera slumpmässiga nummer behöver du först lägga till `rand` i din `Cargo.toml`-fil:

```toml
[dependencies]
rand = "0.8.5"
```

Nästa steg är att generera slumpmässiga nummer med hjälp av `rand` i din Rust-kod. Här är ett exempel på hur man genererar ett slumpmässigt heltal och ett flyttal:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Generera ett slumpmässigt heltal mellan 1 och 10
    let slump_int: i32 = rng.gen_range(1..=10);
    println!("Slumpmässigt heltal: {}", slump_int);
    
    // Generera ett slumpmässigt flyttal mellan 0.0 och 1.0
    let slump_float: f64 = rng.gen::<f64>();
    println!("Slumpmässigt flyttal: {}", slump_float);
}
```

Exempel på utdata kan vara:

```plaintext
Slumpmässigt heltal: 7
Slumpmässigt flyttal: 0.9401077112175732
```

Observera att om programmet körs om produceras olika värden.

## Fördjupning
Generering av slumpmässiga nummer i Rust, som möjliggörs genom `rand` och dess beroenden som `getrandom`, representerar en bred abstraktion över operativsystemets faciliteter och algoritmiska generatorer. Historiskt har slumpmässighet i datorvärlden utvecklats från enkla, förutsägbara algoritmer till komplexa, kryptografiskt säkra metoder. Rusts tillvägagångssätt inkapslar denna utveckling genom dess utbytbara `Rng`-egenskap, som kan stödjas av olika generatorer beroende på den önskade slumpmässighetens kvalitet och prestanda.

För de flesta applikationer ger förlitande på `rand` och systemets RNG en bra balans mellan enkelhet och entropi. Dock, för kryptografiska applikationer, hänvisar crates som `rand` till `getrandom` för seedning, som i sin tur förlitar sig på system-specifika mekanismer (t.ex. `/dev/urandom` på Unix-lika system), och säkerställer kryptografiskt säker slumpmässighet.

Alternativt, om du har specifika behov som inte tillgodoses av `rand`, kan utforskning av andra crates eller implementering av anpassade generatorer baserade på matematiska modeller vara en väg att gå. Trots detta erbjuder `rand` och dess ekosystem för de flesta användningsfall robusta lösningar som är både effektiva och enkla att integrera i Rust-applikationer.

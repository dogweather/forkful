---
title:                "Avrundning av tal"
date:                  2024-01-26T03:46:49.338473-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"

category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att avrunda tal innebär att justera dem till det närmaste hela talet eller en bråkdel med viss precision. Programmerare avrundar tal för att förenkla värden för läsbarhet för människor, för att uppfylla specifikationskrav eller för att minska beräkningsöverhead vid flyttalsoperationer.

## Hur man gör:
Rust gör avrundning till en barnlek. Kolla in dessa metoder för `f32` eller `f64`-typer:

```rust
fn main() {
    let num = 2.34567;

    // Avrunda till närmaste hela tal
    let round = num.round();
    println!("Avrundat: {}", round); // Avrundat: 2

    // Golvet - största heltal mindre än eller lika med talet
    let floor = num.floor();
    println!("Golv: {}", floor); // Golv: 2

    // Takkvoten - minsta heltal större än eller lika med talet
    let ceil = num.ceil();
    println!("Tak: {}", ceil); // Tak: 3

    // Trunkera - heltalsdelen utan decimaler
    let trunc = num.trunc();
    println!("Trunkerat: {}", trunc); // Trunkerat: 2

    // Närmaste multipel av en tiopotens
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Avrundat till 2 decimaler: {}", multiple_of_ten); // Avrundat till 2 decimaler: 2.35
}
```

## Fördjupning
Historiskt har avrundning varit avgörande för att passa in oändliga decimaler eller irrationella tal i begränsade digitala utrymmen—ett måste för uråldriga datorer med knapphändigt minne. Tänk dig ett abakus men mindre hantverk, mer matte.

Alternativ till de inbyggda Rust-metoderna inkluderar:
1. `format!` makro för strängformatering som avrundar som standard.
2. Externa lådor för specialiserade matematiska uppgifter, som `round`-lådan med mer granulär kontroll.

Bakom kulisserna följer Rusts avrundningsoperationer IEEE-standarder—teknisk jargong för "det avrundar som din mattelärare vill." Dessutom kan på grund av binära representationer vissa tal inte traditionellt avrundas, som 0,1, på grund av deras oändliga representation i binärt.

## Se även
- Rust-dokumentation om primitiva typmetoder: https://doc.rust-lang.org/std/primitive.f64.html
- IEEE-standard för flyttalsaritmetik (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- "round"-lådan för mer komplex avrundning: https://crates.io/crates/round

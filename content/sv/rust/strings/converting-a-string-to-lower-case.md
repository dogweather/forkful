---
date: 2024-01-20 17:39:35.797741-07:00
description: "Hur man g\xF6r: Rust anv\xE4nder metoden `to_lowercase()` f\xF6r att\
  \ konvertera en str\xE4ng till enbart sm\xE5 bokst\xE4ver. H\xE4r \xE4r ett kort\
  \ exempel."
lastmod: '2024-03-13T22:44:37.685048-06:00'
model: gpt-4-1106-preview
summary: "Rust anv\xE4nder metoden `to_lowercase()` f\xF6r att konvertera en str\xE4\
  ng till enbart sm\xE5 bokst\xE4ver."
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

## Hur man gör:
Rust använder metoden `to_lowercase()` för att konvertera en sträng till enbart små bokstäver. Här är ett kort exempel:

```rust
fn main() {
    let original = "Hej Världen!";
    let small_letters = original.to_lowercase();

    println!("Original: {}", original);
    println!("Små bokstäver: {}", small_letters);
}
```
Utdata:
```
Original: Hej Världen!
Små bokstäver: hej världen!
```

## Djupdykning:
Konvertering till gemener i programmeringsspråket Rust hanteras av Unicode, vilket betyder att den globalt sett fungerar för de flesta språk och bokstäver. Historiskt sett var detta inte alltid enkelt, då äldre system ofta endast stödde ASCII-tecken.

Alternativ till `to_lowercase()` kan inkludera egna implementationer, men är vanligtvis onödigt då standardmetoden är väl anpassad och optimerad.

När det gäller implementation så tar `to_lowercase()` hänsyn till lokala variationer och speciella tecken, vilket gör funktionen säker och pålitlig för internationell användning.

## Se även:
- Rusts officiella dokumentation om strängar: [https://doc.rust-lang.org/std/string/](https://doc.rust-lang.org/std/string/)
- Unicode organization: [http://www.unicode.org/](http://www.unicode.org/)
- Rust by Example om strängar: [https://doc.rust-lang.org/rust-by-example/std/str.html](https://doc.rust-lang.org/rust-by-example/std/str.html)

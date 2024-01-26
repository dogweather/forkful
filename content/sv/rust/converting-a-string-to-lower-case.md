---
title:                "Konvertera en sträng till gemener"
date:                  2024-01-20T17:39:35.797741-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att alla stora bokstäver i texten förvandlas till småa bokstäver. Programmerare gör detta för att förenkla jämförelser och sökningar, eftersom skillnader i versalisering då inte spelar någon roll.

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

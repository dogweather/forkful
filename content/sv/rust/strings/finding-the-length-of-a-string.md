---
date: 2024-01-20 17:48:17.820823-07:00
description: "I Rust ger `len()` l\xE4ngden p\xE5 en str\xE4ng, det vill s\xE4ga hur\
  \ m\xE5nga bytes den \xE4r. Vi beh\xF6ver veta storleken f\xF6r att hantera textdata\
  \ effektivt - som att\u2026"
lastmod: '2024-03-13T22:44:37.688927-06:00'
model: gpt-4-1106-preview
summary: "I Rust ger `len()` l\xE4ngden p\xE5 en str\xE4ng, det vill s\xE4ga hur m\xE5\
  nga bytes den \xE4r. Vi beh\xF6ver veta storleken f\xF6r att hantera textdata effektivt\
  \ - som att\u2026"
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
I Rust ger `len()` längden på en sträng, det vill säga hur många bytes den är. Vi behöver veta storleken för att hantera textdata effektivt - som att validera inmatning eller sköta textutskärningar.

## Så här gör du:
```rust
fn main() {
    let text = "Hej, världen!";
    let length = text.len(); // Ta reda på längden
    println!("Längden på strängen är: {}", length);
}
```
Sample output:
```
Längden på strängen är: 13
```

Notera att `len()` räknar bytes och inte tecken, viktigt för text med åäö.

## Fördjupning
Från början hade Rust olika metoder för att hantera stränglängder, men med tiden har standarden blivit `.len()` funktionen. Kom ihåg att `.len()` ger oss antalet bytes i strängen, inte antalet "char" eller tecken. För strängar med enbart ASCII-tecken är detta samma sak, men för Unicode-strängar, kan det vara annorlunda då vissa tecken kan ta flera bytes att representera. Om du behöver antalet karaktärer eller grafemkluster, kan du istället använda `.chars().count()` eller `.graphemes(true).count()` med hjälp av `unicode-segmentation` biblioteket.

Alternativ till `.len()` innefattar iteration över strängen eller att använda externa bibliotek som kan ge mer riktade funktioner för specifika problem.

När det gäller implementeringsdetaljer, lagras utf-8 kodade Rust strängar som en samling bytes, så `.len()` är en snabb operation eftersom längden är datat strukturens storlek, inte resultatet av någon beräkning.

## Se även
- Rusts officiella dokumentation om strängar: https://doc.rust-lang.org/std/string/struct.String.html
- 'unicode-segmentation' på crates.io: https://crates.io/crates/unicode-segmentation
- Rust bok kapitel om strängtyper: https://doc.rust-lang.org/book/ch08-02-strings.html

---
date: 2024-01-20 17:48:17.820823-07:00
description: "S\xE5 h\xE4r g\xF6r du: Fr\xE5n b\xF6rjan hade Rust olika metoder f\xF6\
  r att hantera str\xE4ngl\xE4ngder, men med tiden har standarden blivit `.len()`\
  \ funktionen. Kom ih\xE5g att\u2026"
lastmod: '2024-04-05T22:50:51.977606-06:00'
model: gpt-4-1106-preview
summary: "Fr\xE5n b\xF6rjan hade Rust olika metoder f\xF6r att hantera str\xE4ngl\xE4\
  ngder, men med tiden har standarden blivit `.len()` funktionen."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

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

---
date: 2024-01-20 17:51:30.648021-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:37.684150-06:00'
model: gpt-4-1106-preview
summary: .
title: "Interpolera en str\xE4ng"
weight: 8
---

## Hur man gör:
```Rust
fn main() {
    let name = "Världen";
    println!("Hej, {}!", name); // Standard interpolering
    let age = 28;
    println!("Du är {} år gammal.", age); // Siffror fungerar också
}
```
Utskrift:
```
Hej, Världen!
Du är 28 år gammal.
```

## Djupdykning:
I många språk görs stränginterpolering med specialtecken som `{}` eller `${}`. I Rust använder vi `{}` och format-makrot för detta. Historiskt sett hade man kanske använt format-specifikatorer som i språket C. Alternativ till `println!` kan vara att använda `format!` för att spara den formaterade strängen i en variabel, eller `write!` för att skriva till en mätare. Denna interpolationsmetod är typsäker, vilket innebär att Rust-kompilatorn kommer att säkerställa att data som är inbäddade i strängar är av rätt typ, och detta sker vid kompilering snarare än vid körning.

## Se även:
- Rusts officiella dokumentation om "format strings": https://doc.rust-lang.org/std/fmt/
- `println!` makrodetaljer: https://doc.rust-lang.org/std/macro.println.html
- Översikt över Rusts "macros": https://doc.rust-lang.org/book/ch19-06-macros.html

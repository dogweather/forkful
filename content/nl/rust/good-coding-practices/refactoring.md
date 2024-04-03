---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:22.457384-07:00
description: "Refactoren is het proces van het herstructureren van bestaande computercode\
  \ - het veranderen van de factoring - zonder de externe gedrag ervan te\u2026"
lastmod: '2024-03-13T22:44:50.602842-06:00'
model: gpt-4-0125-preview
summary: Refactoren is het proces van het herstructureren van bestaande computercode
  - het veranderen van de factoring - zonder de externe gedrag ervan te wijzigen.
title: Refactoring
weight: 19
---

## Hoe:
Laten we een eenvoudig stukje Rust-code refactoren om het meer idiomatisch en onderhoudbaar te maken. We beginnen met een functie die de som berekent van een vector van integers:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("De som is {}", sum(&numbers));
}
```

Output:
```
De som is 15
```

Nu, laten we dit refactoren om meer idiomatisch Rust te gebruiken door iterators en de `fold` methode te gebruiken:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("De som is {}", sum(&numbers));
}
```

Geen verandering in output - het is nog steeds `15` - maar de gerefactoreerde versie is schoner en gebruikt de sterke punten van Rust zoals lenen en iterator methoden.

## Diepgaand onderzoek
Refactoren heeft zijn wortels in de Smalltalk-gemeenschap en werd populair gemaakt in de Java-wereld door Martin Fowlers boek "Refactoring: Improving the Design of Existing Code". De principes zijn universeel en gelden ook voor Rust, waar veiligheid en gelijktijdigheid van het grootste belang zijn. Rust moedigt het schrijven van robuuste code aan door problemen tijdens de compilatietijd te vangen, dus tijdens het refactoren fungeert de Rust-compiler als een veiligheidsnet.

Alternatieven voor handmatig refactoren omvatten het gebruik van geautomatiseerde hulpmiddelen, zoals 'rustfmt' voor code-opmaak en 'clippy' voor linting, die meer idiomatische manieren van coderen kunnen suggereren. Echter, diepgaand refactoren vereist vaak een doordacht begrip van het ontwerp van de code, wat deze tools niet volledig kunnen automatiseren.

In Rust kan refactoren draaien rond het verbeteren van het gebruik van typen, effectief gebruikmaken van levensduur, het verminderen van onnodige toewijzingen, of het toepassen van gelijktijdigheidspatronen zoals het gebruik van `Arc<Mutex<T>>` wanneer nodig. Het is ook gebruikelijk om over te gaan van `unwrap()` naar expressievere foutafhandeling met `Result<T, E>`.

## Zie ook
Om verder te duiken in refactoren in Rust:

- Het Rust Boek: https://doc.rust-lang.org/book/
- Rust bij Voorbeeld: https://doc.rust-lang.org/rust-by-example/
- Clippy, een Rust linting tool: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" door Martin Fowler: https://martinfowler.com/books/refactoring.html

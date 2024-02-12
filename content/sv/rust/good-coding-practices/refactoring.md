---
title:                "Refaktorisering"
aliases: - /sv/rust/refactoring.md
date:                  2024-01-26T03:37:17.978527-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Refaktorisering är processen att omstrukturera befintlig dator-kod—ändra faktoriseringen—utan att förändra dess externa beteende. Programmerare gör det för att förbättra icke-funktionella attribut hos mjukvaran, såsom läsbarhet, minskad komplexitet, förbättra underhållbarheten och skapa en mer uttrycksfull intern arkitektur eller objektmodell för att förbättra utbyggbarheten.

## Hur:

Låt oss refaktorisera en enkel bit Rust-kod för att göra den mer idiomatisk och underhållsvänlig. Vi börjar med en funktion som beräknar summan av en vektor med heltal:

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
    println!("Summan är {}", sum(&numbers));
}
```

Utskrift:
```
Summan är 15
```

Nu, låt oss refaktorisera detta för att använda mer idiomatisk Rust genom att utnyttja iteratorer och `fold`-metoden:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Summan är {}", sum(&numbers));
}
```

Ingen förändring i utskriften—det är fortfarande `15`—men den refaktoriserade versionen är renare och använder Rusts styrkor såsom låning och iterator-metoder.

## Fördjupning

Refaktorisering har sina rötter i Smalltalk-gemenskapen och populariserades i Java-världen genom Martin Fowlers bok "Refaktorisering: Att förbättra designen på befintlig kod". Dess principer är universella och tillämpas också på Rust, där säkerhet och samtidighet är av yttersta vikt. Rust uppmuntrar till att skriva robust kod genom att fånga problem vid kompileringstid, så under refaktoriseringen fungerar Rust-kompilatorn som ett säkerhetsnät.

Alternativ till manuell refaktorisering inkluderar användning av automatiserade verktyg, såsom 'rustfmt' för kodformatering och 'clippy' för lintning, som kan föreslå mer idiomatiska sätt att skriva kod på. Dock kräver djup refaktorisering ofta en genomtänkt förståelse för kodens design, något som dessa verktyg inte kan automatisera helt och hållet.

I Rust kan refaktorisering involvera förbättringar av typanvändning, effektiv användning av livstider, minskning av onödiga allokeringar eller användning av samtidighetsmönster som att använda `Arc<Mutex<T>>` när det behövs. Det är också vanligt att gå från `unwrap()` till mer uttrycksfull felhantering med `Result<T, E>`.

## Se även

För att fördjupa dig ytterligare i refaktorisering i Rust:

- Rust-boken: https://doc.rust-lang.org/book/
- Rust genom exempel: https://doc.rust-lang.org/rust-by-example/
- Clippy, ett lint-verktyg för Rust: https://github.com/rust-lang/rust-clippy
- "Refaktorisering: Att förbättra designen på befintlig kod" av Martin Fowler: https://martinfowler.com/books/refactoring.html

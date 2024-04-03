---
date: 2024-01-20 18:04:22.817555-07:00
description: "How to: (Jak to zrobi\u0107:) Zacznij od Cargo, narz\u0119dzia Rust\
  \ do zarz\u0105dzania projektami."
lastmod: '2024-03-13T22:44:35.185368-06:00'
model: gpt-4-1106-preview
summary: "Zacznij od Cargo, narz\u0119dzia Rust do zarz\u0105dzania projektami."
title: Rozpoczynanie nowego projektu
weight: 1
---

## How to: (Jak to zrobić:)
Zacznij od Cargo, narzędzia Rust do zarządzania projektami.

```Rust
// Zainstaluj Rust i Cargo (ten drugi jest dostarczany z Rustem)
// Na otwórz terminal i wykonaj:

cargo new moj_projekt
cd moj_projekt
cargo run

// Spodziewany wynik:
//   Compiling moj_projekt v0.1.0 (/ścieżka/do/moj_projekt)
//    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
//     Running `target/debug/moj_projekt`
// Hello, world!
```

## Deep Dive (Dogłębna analiza)
Cargo, menedżer projektów i system buildowania Rusta, został wprowadzony w 2014 roku. Alternatywą jest ręczne tworzenie plików i używanie `rustc` do kompilacji, ale to mniej wydajne. Proces uruchamiania nowego projektu Cargo obejmuje stworzenie struktury z `Cargo.toml` i katalogiem `src`. Dzięki temu, Rabo przy zarządzaniu zależnościami i budową skomplikowanych aplikacji.

## See Also (Zobacz też)
- [The Cargo Book](https://doc.rust-lang.org/cargo/) - oficjalna dokumentacja Cargo.
- [Rustaceans.org](https://www.rustaceans.org/) - społeczność Rust, gdzie można zadawać pytania.
- [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/) - nauka Rusta na praktycznych przykładach.

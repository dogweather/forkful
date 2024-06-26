---
date: 2024-01-20 17:46:21.402076-07:00
description: "How to: Historiquement, l'extraction de sous-cha\xEEnes en Rust peut\
  \ \xEAtre pi\xE8geuse car elle op\xE8re directement avec les index des octets et\
  \ non des\u2026"
lastmod: '2024-04-05T21:53:59.033725-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, l'extraction de sous-cha\xEEnes en Rust peut \xEAtre pi\xE8\
  geuse car elle op\xE8re directement avec les index des octets et non des caract\xE8\
  res."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## How to:
```Rust
fn main() {
    let text = String::from("Bonjour, Rustaceans!");
    let hello = &text[0..7]; // "Bonjour"
    let rustaceans = &text[9..]; // "Rustaceans!"

    println!("Salut: {}", hello);
    println!("Fans de Rust: {}", rustaceans);
}
```
Sortie:
```
Salut: Bonjour
Fans de Rust: Rustaceans!
```

## Deep Dive
Historiquement, l'extraction de sous-chaînes en Rust peut être piègeuse car elle opère directement avec les index des octets et non des caractères. Cela peut entraîner des erreurs si l'on ne fait pas attention aux points de code UTF-8 qui prennent plus d'un octet. À défaut d'utiliser la syntaxe de tranches, il existe des méthodes comme `slice::split_at()` ou des itérateurs comme `char_indices()`, qui sont plus sûrs avec l'encodage UTF-8 de Rust.

Les détails d'implémentation sont cruciaux: une extraction incorrecte peut causer le plantage du programme si on essaie de couper un caractère multioctet. Il est important de toujours valider ou de connaître les limites des données que l’on manipule.

## See Also
- [La documentation officielle de Rust sur les slices de chaînes](https://doc.rust-lang.org/std/string/struct.String.html#method.as_bytes)
- [Le module `std::str` pour plus d’opérations sur les chaînes de caractères](https://doc.rust-lang.org/std/str/)
- [Discussion sur les problèmes d’unicode avec les slices sur users.rust-lang.org](https://users.rust-lang.org/t/how-to-get-a-substring-of-a-string/1351)

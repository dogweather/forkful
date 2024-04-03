---
date: 2024-01-20 17:35:25.023138-07:00
description: 'Comment faire : .'
lastmod: '2024-03-13T22:44:57.473698-06:00'
model: gpt-4-1106-preview
summary: .
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## Comment faire :
```Rust
fn main() {
    let salutation = "Salut".to_string();
    let monde = " le monde!";
    let phrase = salutation + monde;

    println!("{}", phrase); // Affiche "Salut le monde!"
}
```
Et avec plusieurs chaînes:
```Rust
fn main() {
    let prenom = "Paul".to_string();
    let espace = " ";
    let nom = "Dupont";
    let message = format!("{}{}{}", prenom, espace, nom);

    println!("{}", message); // Affiche "Paul Dupont"
}
```

## Plongée en profondeur
En Rust, il y a différentes méthodes pour concaténer des chaînes car Rust met l'accent sur la sécurité et l'efficacité de la mémoire. Historiquement, des langages plus anciens comme C étaient moins stricts, ce qui pouvait conduire à des erreurs et des failles de sécurité. Rust préfère que vous utilisiez `format!` pour des concaténations complexes, car cela évite les allocations inutiles de mémoire. En interne, Rust traite les chaînes comme des collections de caractères UTF-8, donc la concaténation implique plus que de simplement "coller" des bits ensemble; il faut aussi gérer l'encodage correctement. Pour les gros travaux de concaténation, envisagez d'utiliser `String::with_capacity` pour minimiser les réallocations.

## Voir aussi
- Rust Book sur la gestion des chaînes de caractères: https://doc.rust-lang.org/book/ch08-02-strings.html
- Documentation Rust pour `format!`: https://doc.rust-lang.org/std/macro.format.html
- Rust by Example sur la concaténation: https://doc.rust-lang.org/rust-by-example/std/str.html

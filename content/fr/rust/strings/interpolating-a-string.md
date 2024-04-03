---
date: 2024-01-20 17:51:43.291698-07:00
description: "L'interpolation de cha\xEEne permet d'ins\xE9rer des variables dans\
  \ des textes. Les programmeurs l'utilisent pour cr\xE9er des messages dynamiques\
  \ et personnaliser\u2026"
lastmod: '2024-03-13T22:44:57.467424-06:00'
model: gpt-4-1106-preview
summary: "L'interpolation de cha\xEEne permet d'ins\xE9rer des variables dans des\
  \ textes."
title: "Interpolation de cha\xEEnes de caract\xE8res"
weight: 8
---

## How to:
```Rust
fn main() {
    let planete = "Terre";
    let population = 7_800_000_000;
    
    // Utilisation de la macro `format!` pour l'interpolation
    let message = format!("Bonjour, habitants de la planète {}! Population: {}", planete, population);
    println!("{}", message);
}
```
Sortie:
```
Bonjour, habitants de la planète Terre! Population: 7800000000
```

## Deep Dive
Historiquement, l'interpolation de chaînes en Rust est réalisée à travers des macros comme `format!`, `print!` ou `println!`, empruntant une syntaxe similaire à celle du langage C pour le formatage (e.g., `%s`, `%d`). Rust n'a pas d'interpolation de chaîne intégrée comme dans d'autres langages (par exemple `"Hello, ${name}!"` en JavaScript). 

Les alternatives incluent l'utilisation de la concaténation ou de bibliothèques tierces. L'implémentation de l'interpolation repose sur le trait `Display` pour convertir les types en chaînes, permettant un affichage convivial. De plus, Rust garantit la sécurité de type lors de l'interpolation, éliminant un grand nombre d'erreurs possibles à l'exécution.

## See Also
- La documentation officielle de Rust sur les macros de formatage: https://doc.rust-lang.org/std/fmt/
- Le trait `Display`: https://doc.rust-lang.org/std/fmt/trait.Display.html
- Un guide pour utiliser `format!`: https://doc.rust-lang.org/stable/rust-by-example/hello/print/fmt.html

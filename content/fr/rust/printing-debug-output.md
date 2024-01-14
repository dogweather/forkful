---
title:    "Rust: Affichage des sorties de débogage"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Pourquoi

L'impression de sortie de débogage est un élément essentiel de tout programmeur expérimenté. Cela peut sembler ennuyeux ou fastidieux, mais croyez-nous, c'est une habitude que vous voudrez développer. L'impression de sortie de débogage vous permet de suivre le fonctionnement de votre code et de détecter rapidement les erreurs.

# Comment Faire

L'impression de sortie de débogage en Rust est aussi simple que d'utiliser la macro `println!`. Cette macro prend en paramètre une chaîne de caractères et les arguments que vous souhaitez afficher. Voyons un exemple :

```Rust
fn main() {
    let nombre = 10;
    println!("Le nombre vaut : {}", nombre);
}
```

Dans cet exemple, nous avons utilisé la macro `println!` pour afficher la valeur de la variable `nombre` en utilisant le format `{}` pour la remplacer. Vous pouvez également utiliser d'autres spécificateurs de format pour afficher différents types de données. Par exemple, `{:.2}` affichera un nombre flottant avec deux décimales.

# Plongée En Profondeur

En utilisant l'impression de sortie de débogage, vous pouvez également afficher le contenu de structures de données plus complexes telles que des vecteurs ou des hashmaps. Vous pouvez également utiliser `debug!` pour obtenir une sortie plus détaillée. Par exemple :

```Rust
fn main() {
    let fruits = vec!["pomme", "banane", "orange"];
    debug!(fruits);
}
```

Cela affichera une sortie telle que `[ "pomme", "banane", "orange" ]` avec des informations supplémentaires sur le type et la taille du vecteur.

# Voir Aussi

- [Documentation officielle de Rust sur l'impression de sortie de débogage](https://doc.rust-lang.org/std/macro.println.html)
- [Article sur l'impression de sortie de débogage en Rust](https://www.educative.io/resources/system-design-for-coding-interviews)
- [Vidéo tutoriel sur l'impression de sortie de débogage en Rust](https://www.youtube.com/watch?v=yG4sX57nKPU)
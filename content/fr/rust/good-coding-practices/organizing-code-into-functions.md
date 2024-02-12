---
title:                "Organiser le code en fonctions"
aliases:
- /fr/rust/organizing-code-into-functions.md
date:                  2024-01-26T01:16:04.627935-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organiser le code en fonctions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Organiser le code en fonctions consiste à décomposer votre programme en morceaux modulaires réutilisables identifiés par un nom. Nous le faisons pour rendre notre code plus propre, plus lisible et plus facile à déboguer. Il s'agit de ne pas se répéter et de rationaliser les mises à jour.

## Comment faire :
Disons que vous avez du code qui calcule l'aire d'un cercle plusieurs fois. Au lieu de répéter la formule, vous l'enveloppez dans une fonction.

```Rust
fn calculate_circle_area(rayon: f64) -> f64 {
    std::f64::consts::PI * rayon.powi(2)
}

fn main() {
    let rayon = 5.0;
    let aire = calculate_circle_area(rayon);
    println!("L'aire du cercle est : {}", aire);
}
```

Sortie :

```
L'aire du cercle est : 78.53981633974483
```

## Plongée profonde
Historiquement, les fonctions viennent des mathématiques, où elles associent des entrées à des sorties. En codage, elles existent depuis l'époque de l'assemblage, bien que nous les appelions "sous-programmes". Les fonctions Rust peuvent retourner des valeurs et même d'autres fonctions grâce aux fonctions de première classe et aux fermetures.

Des alternatives ? Du code inline ou des macros, mais ils sont désordonnés pour une logique complexe. Les objets avec méthodes constituent une autre façon d'organiser la fonctionnalité, une saveur différente des fonctions autonomes.

L'implémentation en Rust est assez simple. Les fonctions déclarent leurs types de paramètres et le type de retour. Leurs noms suivent par convention le "snake case". Vous avez vos fonctions publiques (`pub fn`) pour une utilisation en dehors du module et les privées pour une utilisation interne. Et Rust a cette fonctionnalité cool où vous n'avez pas besoin d'un mot-clé `return` pour la dernière expression dans une fonction.

## Voir aussi
Consultez ces sources pour plus d'infos :
- Le livre sur le langage de programmation Rust : [Fonctions](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust par l'exemple sur les [Fonctions](https://doc.rust-lang.org/rust-by-example/fn.html)

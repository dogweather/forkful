---
title:                "Utiliser les expressions régulières"
html_title:           "Rust: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'utilisation des expressions régulières est un outil essentiel pour les programmeurs. Cela leur permet de trouver et de manipuler facilement des motifs spécifiques dans des chaînes de caractères. Les expressions régulières sont particulièrement utiles pour les tâches de détection de données, de validation de saisie ou de filtrage de résultats.

## Comment faire :

Voici un exemple de code en Rust pour utiliser des expressions régulières :

```
use regex::Regex;

fn main() {
    // Définir le motif
    let re = Regex::new(r"([a-z]+) ([0-9]+)").unwrap();
    
    // La chaîne de caractères à tester
    let text = "hello 123";
    
    // Rechercher la correspondance
    if let Some(captures) = re.captures(text) {
        // Afficher les résultats
        println!("Mot trouvé : {}", captures.get(1).unwrap().as_str());
        println!("Nombre trouvé : {}", captures.get(2).unwrap().as_str());
    }
}
```

Sortie :

```
Mot trouvé : hello
Nombre trouvé : 123
```

## Plongée en profondeur :

Les expressions régulières ont été inventées dans les années 1950 par un mathématicien américain, Stephen Kleene, pour décrire certains motifs dans les langages formels. Les alternatives à l'utilisation des expressions régulières incluent l'utilisation de boucles et de méthodes de chaîne de caractères telles que `find` et `replace`. Les expressions régulières en Rust sont implémentées avec la bibliothèque standard `regex`, qui offre une syntaxe similaire à celle des autres langages.

## Voir aussi :

- [Documentation officielle sur les expressions régulières en Rust](https://doc.rust-lang.org/std/regex/index.html)
- [Tutoriel en français sur les expressions régulières en Rust](https://www.azriel.dev/rust_regex_tuto/)
- [Site de référence pour tester et expérimenter des expressions régulières en temps réel](https://regex101.com/)
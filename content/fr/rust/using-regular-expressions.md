---
title:                "Utilisation des expressions régulières"
html_title:           "Rust: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec du texte dans vos projets de programmation, vous avez sûrement rencontré des situations où vous devez rechercher et manipuler des motifs de caractères spécifiques dans une chaîne de texte. Les expressions régulières peuvent vous aider à accomplir cette tâche de manière simple et efficace.

## Comment faire

Les expressions régulières sont supportées nativement par Rust grâce au crate "regex" qui peut être inclus dans votre projet en ajoutant la ligne suivante dans votre fichier "Cargo.toml":

```Rust
[dependencies]
regex = "1.4"
```

Dans l'exemple suivant, nous allons utiliser des expressions régulières pour valider et extraire une adresse email à partir d'une chaîne de texte:

```Rust
use regex::Regex;

fn main() {
    // Définir le motif de l'adresse email recherché
    let email_pattern = Regex::new(r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}").unwrap();

    // Déclarer la chaîne de texte à analyser
    let text = "Mon adresse email est contact@exemple.com";

    // Utiliser la méthode "find" pour rechercher le motif dans la chaîne de texte
    if let Some(email) = email_pattern.find(text) {
        println!("Email trouvé: {}", email.as_str()); // Obtenir la correspondance sous forme de texte
    } else {
        println!("Aucune adresse email trouvée.");
    }
}
```

Sortie:
```
Email trouvé: contact@exemple.com
```

## Plongée en profondeur

Les motifs de recherche dans les expressions régulières sont définis à l'aide de caractères spéciaux et de symboles qui ont des significations particulières. Voici quelques-uns des symboles les plus couramment utilisés:

- `.` : correspond à n'importe quel caractère unique
- `*` : correspond à zéro ou plusieurs occurrences du caractère précédent
- `+` : correspond à une ou plusieurs occurrences du caractère précédent
- `?` : correspond à zéro ou une occurrence du caractère précédent
- `[]` : correspond à un ensemble de caractères possibles
- `()` : permet de regrouper des caractères pour former un motif plus complexe
- `|` : permet de définir des alternatives entre les motifs

Il existe également des séquences d'échappement qui permettent d'utiliser des caractères spéciaux dans des motifs qui ont une signification différente. Par exemple, `\d` correspond à un chiffre et `\w` correspond à un caractère alphanumérique.

Il est recommandé de tester et de valider vos expressions régulières à l'aide d'outils en ligne comme regex101.com avant de les utiliser dans votre code.

## Voir aussi

- La documentation officielle du crate "regex" : https://docs.rs/regex/1.4.2/regex/
- Un tutoriel complet sur les expressions régulières en Rust : https://docs.rs/regex/1.4.2/regex/#getting-started
- Le crate "lazy_static" qui peut améliorer les performances lors de l'utilisation d'expressions régulières : https://docs.rs/lazy_static/1.4.0/lazy_static/
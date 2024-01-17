---
title:                "Travailler avec yaml"
html_title:           "Rust: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

Qu'est-ce que YAML et pourquoi les programmeurs l'utilisent-ils?

YAML, acronyme de "YAML Ain't Markup Language", est un format de données qui est couramment utilisé pour stocker et transmettre des données structurées. Les programmeurs l'utilisent pour sa simplicité et sa flexibilité, car les données peuvent être facilement lues et écrites par les humains sans nécessiter de logiciel spécialisé.

Comment utiliser YAML en Rust:

```rust
// Exemple de lecture d'un fichier YAML
let contenu_fichier = std::fs::read_to_string("fichier.yaml")?;
let donnees: serde_yaml::Value = serde_yaml::from_str(&contenu_fichier)?;

// Affichage des données
println!("Valeurs: {:?}", donnees);

// Exemple d'écriture d'un fichier YAML
let donnees = serde_yaml::to_string(&valeurs)?;
std::fs::write("nouveau_fichier.yaml", &donnees)?;

```

Sortie:

```
Valeurs: Some({"nom":"Jean","age":27,"ville":"Paris"})
```

Plongée en profondeur:

Le format YAML a été créé en 2001 dans le but de remplacer le format XML, qui est souvent considéré comme trop verbeux et difficile à lire. YAML est plus facile à lire pour les humains car il utilise une syntaxe basée sur l'indentation et des balises claires. Il a également des similitudes avec le format JSON, mais avec plus de fonctionnalités telles que la prise en charge des commentaires.

Il existe d'autres alternatives à YAML telles que TOML et ini, mais YAML est devenu un standard de facto pour la configuration et la transmission de données structurées. Il est également pris en charge par de nombreuses bibliothèques et langages de programmation tels que Python, Java et bien sûr Rust grâce à la bibliothèque serde_yaml.

Pour utiliser YAML en Rust, nous pouvons utiliser la bibliothèque serde_yaml, qui fournit des fonctions pour lire et écrire des données YAML de manière simple et efficace. Le résultat de la lecture d'un fichier YAML est généralement une valeur de type serde_yaml::Value, qui peut ensuite être manipulée selon les besoins.

Voir aussi:

Pour en savoir plus sur YAML et son utilisation en Rust, vous pouvez consulter la documentation officielle de serde_yaml ici: https://docs.serde.rs/serde_yaml/

Pour découvrir d'autres alternatives à YAML en Rust, vous pouvez également consulter la bibliothèque toml-rs ici: https://github.com/alexcrichton/toml-rs et la bibliothèque ini-rs ici: https://github.com/zonyitoo/ini-rs
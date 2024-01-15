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

## Pourquoi

Si vous êtes un développeur en herbe ou vétéran, vous avez probablement entendu parler de YAML. Il s'agit d'un langage simple de sérialisation de données utilisé pour stocker et échanger des informations structurées. Cela peut sembler intimidant au début, mais en utilisant Rust, vous pouvez facilement manipuler des fichiers YAML et les intégrer dans vos projets.

## Comment faire

Tout d'abord, vous devez importer la bibliothèque YAML dans votre projet Rust. Ajoutez simplement `yaml = "0.4.5"` à votre fichier `Cargo.toml`.

Ensuite, utilisez la méthode `load_from_str` pour charger un fichier YAML dans une variable. Vous pouvez également utiliser `load_from_file` si vous souhaitez charger un fichier YAML externe. Voici un exemple de code:

```Rust
use yaml::{Value, Mapping};

fn main() {
    let yaml_string = "age: 25
name: John Doe";

    let yaml: Value = yaml::load_from_str(yaml_string).unwrap();

    let age = yaml["age"].as_i64().unwrap();
    let name = yaml["name"].as_str().unwrap();

    println!("{} is {} years old", name, age);
}
```

La sortie de ce code sera "John Doe a 25 ans". Vous pouvez également utiliser des structures comme `Mapping` et `Sequence` pour accéder aux différentes valeurs dans le fichier YAML. Consultez la documentation de la bibliothèque YAML pour en savoir plus.

## Plongée en profondeur

Il est important de noter que Rust n'a pas de type de données natif pour les structures de données YAML. Cela signifie que vous devez utiliser la bibliothèque YAML pour effectuer toutes les opérations sur les fichiers YAML.

De plus, lors de la manipulation de données YAML, vous devez faire attention à la structure de votre fichier. YAML est sensible à l'indentation, il est donc important de maintenir la même indentation pour des données similaires. Sinon, vous risquez de rencontrer des erreurs lors de la lecture du fichier.

Enfin, la bibliothèque YAML peut être complexe pour les débutants. N'hésitez pas à consulter des tutoriels et des exemples pour vous familiariser avec son utilisation.

## Voir aussi

- [Documentation de la bibliothèque YAML pour Rust](https://docs.rs/yaml/0.4.5/yaml/)
- [Tutoriel sur la manipulation de fichiers YAML en Rust](https://dev.to/enigeneer/working-with-yaml-files-in-rust-24n5)
- [Exemples de code pour l'utilisation de la bibliothèque YAML en Rust](https://github.com/dtolnay/yaml-rust/tree/master/examples)
---
title:                "Travailler avec yaml"
html_title:           "Gleam: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Travailler avec YAML est un moyen pour les programmeurs de stocker et de gérer des données dans un format facilement lisible par les humains et les machines. Il est souvent utilisé pour configurer des applications ou des infrastructures, car il est léger, extensible et facile à comprendre.

## Comment faire:
Voici un exemple de code en Gleam pour écrire et lire des données YAML:

```Gleam
import gleam/yaml

let data = make_gleam_yaml_data([
  {"name": "John", "age": 25},
  {"name": "Jane", "age": 30}
])

let yaml = data
  |> gleam_io/file.write("my_data.yaml")

let read_yaml = yaml
  |> gleam_io/file.read()
  |> unwrap_just

gleam_yaml.from_string(read_yaml)
|> Ok
|> expect_equal(data)
```

Voici une sortie d'exemple d'une structure de données YAML:

```Gleam
{ name: "John", age: 25 }
```

## Plongée en profondeur:
La première version de YAML a été créée en 2001 par Clark Evans, Ingy döt Net et Oren Ben-Kiki. Il a été conçu pour être orienté objet et facile à comprendre pour les êtres humains, avec une syntaxe indentée qui rappelle les fichiers de configuration INI. YAML est également utilisé pour échanger des données entre différentes langues de programmation.

D'autres alternatives à YAML existent, telles que JSON et XML, mais YAML est souvent privilé comme format de données car il offre à la fois la lisibilité humaine et la fonctionnalité orientée objet.

## À voir également:
- Documentation officielle YAML: https://yaml.org/
- Bibliothèque Gleam YAML: https://gleam.run/packages/gleam_yaml/0.6.0/
- Tutoriel Gleam: https://gleam.run/getting-started/
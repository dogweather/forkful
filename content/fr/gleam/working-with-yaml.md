---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec YAML, c'est manipuler un format de données lisible par l'humain pour la configuration et l'échange d'information. Les programmeurs l'adoptent pour sa simplicité et sa clarté, surtout dans les projets de logiciels avec des fichiers de configuration.

## How to:
Gleam ne dispose pas d'une bibliothèque standard pour analyser YAML, mais on peut interagir avec des bibliothèques externes en utilisant le FFI (Foreign Function Interface). Voici comment vous pourriez le faire :

```gleam
// Imaginez que nous avons une bibliothèque externe `yaml_gleam`
import yaml_gleam

pub fn read_yaml() {
  let content: String = "- name: John Doe\n  age: 30\n- name: Jane Smith\n  age: 25"
  let users = yaml_gleam.parse(content)
  case users {
    Ok(data) -> data
    Error(err) -> err
  }
}
```

Sortie échantillon (en supposant une implémentation qui renvoie une liste de maps) :

```plaintext
[
  #{"name": "John Doe", "age": 30},
  #{"name": "Jane Smith", "age": 25}
]
```

## Deep Dive
Historiquement, YAML est né comme une alternative au XML, plus facile à lire et à écrire. Parmi les alternatives, on retrouve JSON, souvent favorisé pour les APIs web pour sa compatibilité avec JavaScript. En Gleam, on pourrait concevoir une analyse YAML en mappant directement les structures sur des types Gleam, mais les détails dépendent de la bibliothèque choisie et de son API.

## See Also
- La documentation officielle de YAML : https://yaml.org
- Le repo Gleam pour chercher des bibliothèques : https://github.com/gleam-lang
- Un tutoriel JSON avec Gleam pour comprendre la manipulation de formats de données similaires : https://gleam.run/book/tour/json.html

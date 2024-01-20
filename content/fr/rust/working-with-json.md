---
title:                "Travailler avec json"
html_title:           "Rust: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/working-with-json.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

Travailler avec JSON, abréviation de JavaScript Object Notation, est courant pour les programmeurs. Il s'agit d'un format de données léger, facile à lire et à écrire pour les humains, mais également facile à analyser et à générer pour les machines. Cela en fait un choix populaire pour le stockage et l'échange de données structurées.

# Comment faire:

Voici quelques exemples de code en utilisant Rust pour travailler avec JSON:

Un json simple peut être analysé en utilisant la fonction `from_str` du module `json`:

```
use std::str::FromStr;
use rustc_serialize::json::Json;

let json = "{\"name\": \"John\", \"age\": 30}";
let data = Json::from_str(json).unwrap();

println!("Name: {}, Age: {}", data["name"].as_string().unwrap(), data["age"].as_u64().unwrap());
```

Cela produira une sortie comme celle-ci:

```
Name: John, Age: 30
```

Pour générer du JSON, nous pouvons utiliser la méthode `to_string` de la structure `Json`:

```
use rustc_serialize::json::{Object, Json};

let mut json_obj = Object::new();

json_obj.insert("name".to_string(), Json::String("John".to_string()));
json_obj.insert("age".to_string(), Json::U64(30));

let json_str = Json::Object(json_obj).to_string();
println!("{}", json_str);
```

Cela produira une sortie comme celle-ci:

```
{"name":"John","age":30}
```

# Plongeon en profondeur:

JSON a été inventé par Douglas Crockford en 2001 et a depuis gagné en popularité en tant que format de données dans de nombreux langages de programmation, y compris Rust. Bien qu'il existe d'autres formats de données tels que XML, YAML et CSV, JSON est souvent préféré pour sa simplicité et sa compatibilité avec JavaScript.

La plupart des échanges d'informations entre un serveur et un client Web se font en utilisant JSON, ce qui en fait un outil incontournable pour les développeurs Web. Rust dispose d'un support intégré pour JSON avec le module `rustc_serialize`, mais il existe également d'autres bibliothèques telles que `serde_json` pour ceux qui recherchent plus de fonctionnalités.

# Voir aussi:

Voici quelques ressources supplémentaires utiles pour en savoir plus sur la manipulation de JSON en utilisant Rust:

- Documentation officielle de Rust pour le module `serde_json`: [https://docs.serde.rs/serde_json/](https://docs.serde.rs/serde_json/)
- Article de Douglas Crockford sur l'histoire et la spécification de JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
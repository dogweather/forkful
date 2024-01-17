---
title:                "Travailler avec Json"
html_title:           "Elixir: Travailler avec Json"
simple_title:         "Travailler avec Json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## De quoi s'agit-il et pourquoi le faire?
Travailler avec JSON signifie manipuler et échanger des données au format JSON (JavaScript Object Notation). Les programmeurs utilisent souvent JSON car c'est un format de données léger, facile à lire et à écrire.

## Comment faire :
Utiliser JSON en Elixir est très simple grâce à la bibliothèque native `Jason`. Voici un exemple pour parser une chaîne JSON en un objet Elixir :

```Elixir 
json_string = "{\"name\": \"John\", \"age\": 30}"
json_obj = Jason.decode!(json_string) # renvoie {:ok, %{"name" => "John", "age" => 30}}
```

Vous pouvez également encoder un objet Elixir en JSON en utilisant `Jason.encode!/1` :

```Elixir 
elixir_obj = %{name: "Jane", age: 25}
json = Jason.encode!(elixir_obj) # renvoie "{\"name\":\"Jane\",\"age\":25}"
```

## Plongée en profondeur :
JSON a été développé par Douglas Crockford en 2001 pour permettre un échange de données facile entre les applications web. Les alternatives à JSON incluent XML, YAML et BSON. La bibliothèque `Jason` est basée sur une implémentation de type JSON qui utilise des dictionnaires Elixir pour représenter les objets JSON.

## A voir également :
- Documentation de la bibliothèque Jason : https://hexdocs.pm/jason/readme.html
- "Introduction to JSON" par Douglas Crockford : https://www.json.org/json-fr.html
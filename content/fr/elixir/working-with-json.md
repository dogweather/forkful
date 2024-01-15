---
title:                "Travailler avec json"
html_title:           "Elixir: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données structurées, il est probable que vous soyez déjà familiarisé avec le format JSON. Il s'agit d'un format populaire pour échanger des données entre différentes applications. Utiliser Elixir pour manipuler ces données JSON peut vous offrir une syntaxe claire et concise, ainsi que des performances élevées grâce à sa capacité de manipulation de données immuables.

## Comment faire

Pour commencer à travailler avec JSON en Elixir, vous devrez d'abord importer le module `Poison` en utilisant la directive `import`. Ensuite, utilisez la fonction `decode` pour convertir une chaîne de caractères JSON en une structure de données Elixir.

```Elixir
# Import du module Poison
import Poison

# Décode une chaîne JSON en structure de données Elixir
decoded_data = decode("{\"name\": \"John\", \"age\": 30}")

# Vous pouvez maintenant accéder aux valeurs comme avec n'importe quelle structure Elixir
IO.puts(decoded_data.name) # affiche "John"
IO.puts(decoded_data.age) # affiche 30
```

Si vous souhaitez plutôt convertir une structure de données Elixir en JSON, utilisez la fonction `encode`.

```Elixir
# Définition d'une structure Elixir
data = %{name: "Jane", age: 25}

# Conversion en JSON
encoded_data = encode(data)
IO.puts(encoded_data) # affiche "{\"name\": \"Jane\", \"age\": 25}"
```

Vous pouvez également utiliser des schémas de validation avec JSON en utilisant le module `Jason.Schema`. Cela vous permet de valider et de transformer vos données JSON en une structure de données Elixir en une seule étape.

```Elixir
# Import du module Jason.Schema
import Jason.Schema

# Définition d'un schéma pour nos données
schema = %{name: string, age: integer}

# Validation et transformation des données
validated_data = schema |> validate(pcoded_data)
IO.puts(validated_data.name) # affiche "John"
IO.puts(validated_data.age) # affiche 30
```

## Zoom sur

Il est important de noter que Json est un format de données sans type, ce qui peut rendre la manipulation de données plus complexe. Cependant, Elixir offre des outils puissants tels que les schémas de validation pour faciliter cette tâche. De plus, Elixir prend en charge les données immuables, ce qui peut améliorer les performances lors de la manipulation de données JSON.

## Voir aussi

- [Documentation officielle d'Elixir pour les opérations JSON](https://hexdocs.pm/elixir/master/Pool.html)
- [Exemple de projet utilisant Jason pour manipuler des données JSON](https://github.com/meh/elixir-jason-example)
- [Article sur les schémas de validation avec Jason](https://blog.sweetcode.io/jason-schema-elixir-6378b4b6942d)
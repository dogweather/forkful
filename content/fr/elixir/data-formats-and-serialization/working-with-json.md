---
title:                "Travailler avec JSON"
aliases:
- fr/elixir/working-with-json.md
date:                  2024-02-03T19:22:08.275751-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Travailler avec JSON implique l'analyse de chaînes formatées JSON en structures de données que Elixir peut manipuler, et la sérialisation de structures de données Elixir de retour en chaînes JSON. Cela est essentiel pour le développement web, les API et les fichiers de configuration, car JSON est un format d'échange de données léger, basé sur le texte, indépendant de la langue, largement utilisé pour sa simplicité et sa lisibilité humaine.

## Comment faire :

Dans Elixir, vous pouvez utiliser la bibliothèque `Jason`, un choix populaire pour l'analyse et la génération JSON. Tout d'abord, ajoutez `Jason` aux dépendances de votre projet dans `mix.exs` :

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Ensuite, exécutez `mix deps.get` pour récupérer la dépendance.

### Analyse de JSON :
Pour convertir une chaîne JSON en structures de données Elixir :

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Sortie : %{"name" => "John", "age" => 30}
```

### Génération de JSON :
Pour convertir une map Elixir en une chaîne JSON :

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Sortie : {"age":25,"name":"Jane"}
```

### Travailler avec des Structs :
Pour coder une struct Elixir, vous devez implémenter le protocole `Jason.Encoder` pour votre struct. Voici un exemple :

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Sortie : {"age":28,"name":"Mike"}
```

Cette approche simple vous permettra de commencer à intégrer le traitement JSON dans vos applications Elixir, facilitant l'échange de données dans divers environnements de programmation.

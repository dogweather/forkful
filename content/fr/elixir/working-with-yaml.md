---
title:                "Elixir: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes novice en programmation ou si vous cherchez à apprendre un nouveau langage, vous vous demandez probablement pourquoi vous devriez travailler avec YAML (YAML Ain't Markup Language). En tant que programmeur Elixir, travailler avec YAML peut vous apporter de nombreux avantages, notamment une meilleure lisibilité et une plus grande facilité dans la gestion des données. 

## Comment faire

Commençons par comprendre les bases de YAML. YAML est un format de données qui est souvent utilisé pour stocker et échanger des données entre différentes applications. Il est souvent utilisé pour les fichiers de configuration car il est facile à lire et à écrire. En Elixir, vous pouvez facilement travailler avec YAML en utilisant le module `YAML` et la fonction `decode`. 

Voici un exemple de code pour lire un fichier YAML et en extraire les données : 

```elixir
require YAML

{:ok, data} = File.read("config.yml") |> YAML.decode()
```

Et voici un exemple de configuration YAML : 

```elixir
port: 8080
host: "localhost"
database:
  username: "john"
  password: "secret"
```

## Plongée en profondeur

Pour aller encore plus loin, vous pouvez également utiliser le module `YAML` pour encoder des données en YAML. Vous pouvez également personnaliser votre encodage en utilisant des options telles que `implicit_binary`, `explicit_pairs` ou `sort_map`. 

De plus, le module `YAML` vous permet également de traiter des fichiers YAML imbriqués, ce qui peut être très utile pour gérer des structures de données complexes. 

Enfin, il est important de noter que YAML a ses propres règles syntaxiques, donc il est important de bien comprendre comment elles fonctionnent avant de commencer à travailler avec des fichiers YAML. 

## Voir aussi 

- [Documentation officielle Elixir sur YAML](https://hexdocs.pm/elixir/YAML.html)
- [Site officiel de YAML](https://yaml.org/)
- [Article sur le format YAML sur Medium](https://medium.com/@dejanjakimovski/what-is-yaml-and-why-would-you-use-it-cb3fc40059e2)
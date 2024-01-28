---
title:                "Travailler avec TOML"
date:                  2024-01-26T04:20:44.754193-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Travailler avec TOML signifie analyser et générer des données TOML (Tom's Obvious, Minimal Language) en utilisant Elixir. Les programmeurs l'utilisent pour gérer les fichiers de configuration car TOML est lisible, facile à analyser et se cartographie bien à une structure de données de type hash.

## Comment faire :
Tout d'abord, ajoutez un parseur TOML à vos dépendances mix. Cet exemple utilise `toml-elixir` :

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Lire un fichier TOML :

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Pour convertir des données Elixir en TOML :

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Exemple de sortie :

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Zoom sur
TOML a été créé par Tom Preston-Werner, co-fondateur de GitHub, pour une utilisation dans les fichiers de configuration. Il est conçu pour être plus simple que XML et plus concis que YAML tout en maintenant une cohérence.

Les alternatives incluent les fichiers JSON, YAML et INI, chacun avec ses compromis en termes de lisibilité humaine et de compatibilité de structure de données. TOML excelle dans la représentation claire des données tabulaires et le regroupement imbriqué de données.

Dans Elixir, la gestion de TOML dépend des bibliothèques de décodage et d'encodage, qui transforment les chaînes TOML en maps Elixir et vice versa. L'analyse fonctionne en faisant correspondre les règles de syntaxe de TOML et en les convertissant en types de données d'Elixir. L'encodage fait l'inverse en mappant les types de données d'Elixir de retour à une syntaxe TOML valide.

## Voir aussi
- Langage TOML : https://toml.io/en/
- Dépôt GitHub de `toml-elixir` : https://github.com/bitwalker/toml-elixir
- Détails du paquet Hex pour `toml-elixir` : https://hex.pm/packages/toml_elixir

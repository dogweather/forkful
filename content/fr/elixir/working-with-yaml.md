---
title:                "Travailler avec yaml"
html_title:           "Elixir: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Travailler avec YAML, c'est écrire du code pour créer et gérer des données structurées dans un fichier texte lisible par les humains. Cela peut sembler fastidieux, mais les programmeurs le font pour faciliter la configuration et le stockage de données dans leurs applications.

## How to:

Pour travailler avec YAML dans Elixir, vous pouvez utiliser le module `Yaml` de la librairie standard Elixir. Voici un exemple de code pour lire un fichier YAML :

```Elixir
file = File.read!("config.yml")
data = Yaml.decode(file)
```

Cela va lire le fichier `config.yml` et convertir son contenu en un format de données utilisable en Elixir. Vous pouvez également utiliser `Yaml.encode` pour écrire des données Elixir dans un fichier YAML.

## Deep Dive:

YAML a été créé en 2001 pour résoudre les problèmes de formatage et de lisibilité du format XML. Il est souvent utilisé dans des frameworks web tels que Ruby on Rails pour gérer la configuration des applications et des bases de données.

Les alternatives à YAML incluent JSON et XML, cependant YAML offre une syntaxe plus lisible pour les humains et est mieux adapté pour les fichiers de configuration.

L'implémentation du module `Yaml` en Elixir est basée sur la librairie C `libyaml`, qui est connue pour être rapide et fiable. En outre, le module Elixir inclut des fonctions pour la validation et la manipulation de données YAML.

## See Also:

Pour en savoir plus sur le module `Yaml` en Elixir, vous pouvez consulter la documentation officielle : https://hexdocs.pm/elixir/Yaml.html

Pour explorer d'autres formats de données pris en charge par Elixir, jetez un coup d'oeil à la librairie `Poison` pour JSON et `SweetXML` pour XML.
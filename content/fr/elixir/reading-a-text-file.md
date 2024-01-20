---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi? 
Lire un fichier texte en programmation, c'est extraire les informations d'un fichier sous forme de texte. Les programmeurs le font pour récupérer des données, analyser des logs, ou même pour lire du code source.

## Comment faire:

En Elixir, lire un fichier texte est simple. Par exemple :

```elixir
{:ok, data} = File.read("votre_fichier.txt")
IO.puts(data)
```
La sortie pourrait ressembler à cela, dépendant de votre fichier texte :
```elixir
Ceci est une démo pour lire un fichier.
```

## Approfondissement

Historiquement, la lecture de fichiers est une tâche basique en programmation. Elixir, basé sur Erlang, offre une approche concurrente, ce qui est bénéfique pour lire des fichiers volumineux.

Il existe un certain nombre d'alternatives pour lire un fichier texte en Elixir, comme utiliser `File.stream!`. Cela peut être utile pour lire de gros fichiers ligne par ligne afin d'économiser la mémoire.

Concernant l'implémentation, `File.read` utilise sous le capot le module Erlang `:file` qui fournit des opérations de bas niveau sur les fichiers.

## Voir aussi

Pour des informations supplémentaires, consultez ces liens :
- Documentation officielle Elixir pour le module `File` ([https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)).
- Tutoriel vidéo sur la lecture et l'écriture de fichiers en Elixir ([https://www.youtube.com/watch?v=pOhVj0Mhu1M](https://www.youtube.com/watch?v=pOhVj0Mhu1M)).
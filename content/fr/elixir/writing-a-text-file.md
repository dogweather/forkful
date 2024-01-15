---
title:                "Écrire un fichier texte"
html_title:           "Elixir: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Elixir?

Il y a plusieurs raisons possibles pour lesquelles vous pourriez vouloir écrire un fichier texte en utilisant Elixir. Peut-être que vous voulez créer un journal de bord pour suivre les modifications de votre programme, ou peut-être que vous voulez générer des rapports pour votre application.

## Comment faire:

Pour écrire un fichier texte en Elixir, vous aurez besoin de la fonction `File.write/2`. Voici un exemple de code pour écrire une phrase simple dans un fichier nommé "exemple.txt":

```Elixir
File.write("exemple.txt", "Bonjour le monde")
```

Le résultat sera un fichier texte contenant la phrase "Bonjour le monde". Vous pouvez également utiliser des variables dans le deuxième argument pour écrire du contenu dynamique.

## Plongez plus en profondeur:

La fonction `File.write/2` utilise des options pour contrôler le mode d'écriture du fichier. Par exemple, vous pouvez spécifier si vous voulez écrire à la fin du fichier ou écraser le contenu existant.

Vous pouvez également utiliser d'autres fonctions de gestion de fichiers en Elixir, telles que `File.read/1` pour lire le contenu d'un fichier ou `File.rename/2` pour renommer un fichier.

## Voir aussi:

- [Documentation sur File - Elixir](https://hexdocs.pm/elixir/File.html)
- [5 façons d'utiliser les fichiers en Elixir](https://dev.to/sneha5/5-ways-to-use-files-in-elixir-h04)
- [Gestion de fichiers en Elixir - Tutoriel en ligne](https://elixirschool.com/fr/lessons/basics/io/)
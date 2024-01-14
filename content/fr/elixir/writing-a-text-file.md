---
title:                "Elixir: Ecrire un fichier texte"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire des fichiers texte est un concept fondamental de la programmation Elixir. Cela vous permet de sauvegarder et d'organiser des informations de manière efficace. Que vous cherchiez à créer une application ou simplement à stocker des données, écrire des fichiers texte peut être très utile.

## Comment faire

Pour écrire un fichier texte en Elixir, vous devez d'abord ouvrir un nouveau projet en utilisant `mix new nom_projet`. Ensuite, créez un nouveau fichier avec `touch nom_fichier.txt`. Une fois que vous avez créé le fichier, vous pouvez écrire du contenu en utilisant `File.write!`, suivi du nom du fichier et du contenu entre guillemets, comme dans l'exemple ci-dessous :

```Elixir
File.write!("nom_fichier.txt", "Bonjour! Ceci est un exemple de texte écrit en Elixir.")
```

Vous pouvez également utiliser `IO.puts` pour afficher du contenu à la fois dans le terminal et dans le fichier. Par exemple :

```Elixir
IO.puts("Bonjour!")
File.write!("nom_fichier.txt", "Bonjour!")
```

Cela écrira "Bonjour!" à la fois dans le terminal et dans le fichier.

## Plongée en profondeur

Si vous avez besoin de modifier un fichier existant plutôt que d'en créer un nouveau, vous pouvez utiliser `File.open` pour ouvrir le fichier, `IO.read` pour lire son contenu et `File.write!` pour écrire des modifications. Vous pouvez également utiliser `File.append!` pour ajouter du contenu à la fin du fichier.

Par ailleurs, vous pouvez également utiliser `File.read!` pour lire un fichier et stocker son contenu dans une variable. Vous pouvez ensuite manipuler cette variable comme bon vous semble avant de l'écrire à nouveau avec `File.write!`.

Il est également important de noter que vous devez gérer les erreurs lors de l'écriture de fichiers en utilisant des blocs `try` et `rescue`. Cela permet de s'assurer que votre programme ne se bloque pas en cas d'erreur.

## Voir aussi
- [Documentation sur l'écriture de fichiers en Elixir](https://hexdocs.pm/elixir/File.html)
- [Tutoriel sur les fichiers en Elixir](https://elixirschool.com/fr/lessons/basics/io/)
- [Guide pour écrire des fichiers en Elixir](https://www.tutorialspoint.com/elixir/elixir_file_io.htm)
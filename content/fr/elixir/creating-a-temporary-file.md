---
title:                "Création d'un fichier temporaire"
html_title:           "Elixir: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà créé un programme pour effectuer une tâche temporaire, vous savez à quel point cela peut être fastidieux de devoir créer des fichiers temporaires à chaque fois. C'est là qu'entre en jeu la création de fichiers temporaires en Elixir, une solution simple et efficace pour gérer les données temporaires.

## Comment procéder

Dans Elixir, la création de fichiers temporaires peut être réalisée en utilisant la fonction `File.temp_file/1` qui se trouve dans le module `File`. Voici un exemple de code utilisant cette fonction :

```Elixir
file_path = File.temp_file("prefix")
IO.puts file_path
```

Le code ci-dessus va créer un fichier temporaire avec le préfixe "prefix" et cela va renvoyer le chemin complet du fichier créé. Si vous exécutez le code plusieurs fois, vous verrez que chaque fichier temporaire a un nom différent grâce au préfixe unique.

## Plongée en profondeur

Lorsque vous utilisez la fonction `File.temp_file/1`, le fichier temporaire est créé dans le dossier temporaire système par défaut, c'est-à-dire `/tmp` sur la plupart des systèmes Linux. Cependant, vous pouvez spécifier un autre dossier en ajoutant un deuxième argument à la fonction, comme ceci :

```Elixir
file_path = File.temp_file("prefix", "/path/to/directory")
```

De plus, si vous souhaitez ajouter un suffixe au nom du fichier temporaire, vous pouvez le faire en ajoutant un troisième argument à la fonction.

```Elixir
file_path = File.temp_file("prefix", "/path/to/directory", ".txt")
```

Cette fonction renvoie également un tuple contenant le chemin complet du fichier temporaire et un flux binaire représentant le fichier lui-même. Cela peut être utile si vous avez besoin de manipuler le fichier avant de le renvoyer ou de le supprimer.

## Voir aussi

- [Documentation sur la fonction `File.temp_file/1`](https://hexdocs.pm/elixir/File.html#temp_file/1)
- [Documentation sur les fichiers en Elixir](https://elixir-lang.org/getting-started/file.html)
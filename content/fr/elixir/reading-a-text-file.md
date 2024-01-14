---
title:                "Elixir: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi lire un fichier texte en Elixir
Bienvenue dans notre article sur la lecture des fichiers texte en Elixir ! Dans cet article, nous allons vous expliquer pourquoi il est important de savoir comment lire un fichier texte et comment le faire en utilisant le langage de programmation fonctionnel Elixir.

## Comment faire
Tout d'abord, il est important de comprendre que la lecture de fichiers texte est une tâche courante en programmation. Cela peut être utile pour lire des données stockées dans un fichier texte, telles que des configurations de programmes ou des ensembles de données à traiter.

Pour lire un fichier texte en Elixir, nous utiliserons la fonction `File.read!/1`. Cette fonction prend en paramètre le chemin vers le fichier et renvoie le contenu du fichier sous forme de chaîne de caractères. Voyons un exemple concret :

```Elixir
content = File.read!("mon_fichier.txt")
IO.puts content
```

Dans cet exemple, nous utilisons la fonction `IO.puts/1` pour afficher le contenu du fichier sur la console. Bien sûr, vous pouvez utiliser le contenu du fichier pour effectuer d'autres tâches, comme le traitement des données ou l'affichage dans un format différent.

Maintenant, si nous avons un fichier texte avec plusieurs lignes, nous pouvons utiliser la fonction `Enum.each/2` pour parcourir chaque ligne du fichier. Voici un exemple :

```Elixir
File.stream!("mon_fichier.txt")
|> Enum.each(&IO.inspect/1)
```

Dans ce cas, nous utilisons la fonction `File.stream!/1` pour créer un flux de données à partir du fichier, puis nous utilisons la fonction `Enum.each/2` pour parcourir chaque ligne avec la fonction `IO.inspect/1` pour l'afficher sur la console.

## Plongée plus profonde
Si vous souhaitez en savoir plus sur la lecture des fichiers texte en Elixir, il existe quelques points à prendre en compte. Tout d'abord, assurez-vous que le chemin vers le fichier est correct et que vous avez les permissions nécessaires pour y accéder.

Deuxièmement, vous pouvez également utiliser la fonction `File.read/1` à la place de `File.read!/1` si vous ne voulez pas que le programme plante en cas d'erreur lors de la lecture du fichier. Cela renverra un tuple avec le contenu du fichier en premier élément et un code d'erreur en deuxième élément.

Et enfin, il est important de noter que la lecture de gros fichiers peut ralentir votre programme. Dans ce cas, il peut être préférable d'utiliser la fonction `File.stream!/1` en utilisant des opérations de traitement des données paresseuses.

## Voir aussi
- Documentation officielle sur la lecture de fichiers en Elixir : https://elixir-lang.org/getting-started/io-and-the-file-system.html#opening-and-reading-files
- Tutoriel sur la lecture de fichiers en Elixir : https://elixir-lang.org/getting-started/io-and-the-file-system.html#opening-and-reading-files
- Vidéo sur la lecture de fichiers en Elixir : https://www.youtube.com/watch?v=_0rArWmW_iE
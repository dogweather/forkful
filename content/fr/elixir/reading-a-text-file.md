---
title:    "Elixir: Lecture d'un fichier texte"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La lecture de fichiers texte est un aspect essentiel de la programmation en Elixir. Cela permet aux développeurs de manipuler et d'utiliser des données provenant de différentes sources, telles que des fichiers JSON, CSV ou simplement du texte brut. Cet article vous montrera comment lire un fichier texte en utilisant Elixir.

## Comment faire

Pour lire un fichier texte en Elixir, nous utilisons la fonction `File.read/1`. Elle prend en paramètre le chemin d'accès au fichier à lire et retourne son contenu sous forme de binaire.

```Elixir
# Lire le fichier "exemple.txt"
{:ok, contenu} = File.read("exemple.txt")
IO.puts contenu
```
Sortie:

`Ceci est un exemple de fichier texte`

Ainsi, avec seulement une ligne de code grâce à `File.read/1`, nous pouvons obtenir le contenu d'un fichier texte.

## Plongée en profondeur

En utilisant la fonction `File.read/1`, Elixir lit le contenu du fichier en mémoire. Cela signifie qu'il n'est pas recommandé de lire de gros fichiers à l'aide de cette fonction, car cela pourrait surcharger la mémoire de votre ordinateur.

Pour lire de gros fichiers, nous utilisons la fonction `Stream` pour traiter les données en continu et éviter de les stocker en mémoire. Par exemple, nous pouvons utiliser `Stream.map/2` pour transformer chaque ligne du fichier en utilisant une fonction de mapping donnée.

```Elixir
# Lire un gros fichier en utilisant Stream
"exemple_gros_fichier.txt"
|> File.stream!() # Créer un Stream à partir du fichier
|> Stream.map(&String.reverse/1) # Inverser chaque ligne
|> Stream.into(File.stream!("exemple_inverse.txt")) # Écrire les lignes inversées dans un nouveau fichier
```

## Voir aussi

- Documentation Elixir pour `File`: https://hexdocs.pm/elixir/File.html
- Tutoriel sur l'utilisation de Stream en Elixir: https://elixirschool.com/fr/lessons/advanced/stream/
- Guide complet sur la manipulation de fichiers en Elixir: https://elixir-lang.org/getting-started/io-and-the-file-system.html#files
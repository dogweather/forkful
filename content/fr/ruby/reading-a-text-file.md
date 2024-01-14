---
title:                "Ruby: Lecture d'un fichier texte"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte est une tâche courante en programmation. Cela peut être utile pour extraire des données, enregistrer des informations ou simplement afficher du contenu. Si vous utilisez Ruby comme langage de programmation, vous devez savoir comment lire un fichier texte. Heureusement, c'est assez simple à faire et dans cet article, nous allons vous montrer comment.

## Comment faire

La méthode la plus courante pour lire un fichier texte en Ruby est d'utiliser la classe File. Tout d'abord, nous devons ouvrir le fichier en utilisant la méthode `open`.

```Ruby
fichier = File.open('exemple.txt')
```

Ensuite, nous pouvons lire le contenu du fichier en utilisant la méthode `read`. Si nous voulons le stocker dans une variable, nous pouvons le faire en ajoutant un signe égal à la fin.

```Ruby
contenu = fichier.read
```

Nous pouvons également utiliser la méthode `readlines` pour lire chaque ligne du fichier et les stocker dans un tableau.

```Ruby
lignes = fichier.readlines
```

Enfin, n'oubliez pas de fermer le fichier en utilisant la méthode `close` une fois que vous avez terminé de le lire.

```Ruby
fichier.close
```

Voici un exemple complet:

```Ruby
fichier = File.open('exemple.txt')
contenu = fichier.read
ligne = fichier.readlines
fichier.close
```

Le contenu du fichier `exemple.txt` sera stocké dans la variable `contenu` et chaque ligne sera stockée dans le tableau `lignes`.

## Plongée en profondeur

En plus de la méthode `read` et `readlines`, la classe File a d'autres méthodes pour lire un fichier texte. Par exemple, vous pouvez utiliser `each_line` pour itérer sur chaque ligne du fichier sans stocker le contenu dans une variable. Vous pouvez également spécifier le nombre de caractères à lire en utilisant `read(n)`, où n est le nombre de caractères.

De plus, si vous voulez lire un fichier texte sans avoir à l'ouvrir et à le fermer manuellement, vous pouvez utiliser la méthode `File.foreach`. Cela itérera sur chaque ligne du fichier sans nécessiter d'ouverture et de fermeture manuelle du fichier.

# Voir aussi

- [Ruby Documentation on File Class](https://ruby-doc.org/core-2.7.3/File.html)
- [Ruby File Class Cheatsheet](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Reading a Text File in Ruby - Tutorial](https://www.youtube.com/watch?v=VEaNBsktpvg)
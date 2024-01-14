---
title:    "Ruby: Lecture d'un fichier texte"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par la programmation avec Ruby, vous êtes probablement déjà familiarisé avec la lecture de fichiers textes. Mais pour ceux qui débutent, la lecture de fichiers peut sembler intimidante. Dans ce blog, nous allons expliquer pourquoi il est important de savoir lire un fichier texte en Ruby.

## Comment faire

Tout d'abord, il est important de comprendre comment fonctionne la lecture d'un fichier texte en Ruby. Nous pouvons utiliser la méthode `File.open` pour ouvrir un fichier et enregistrer son contenu dans une variable. Ensuite, nous pouvons utiliser la méthode `read` pour lire le contenu du fichier et l'afficher dans la console. Voici un exemple de code pour illustrer cela :

```Ruby
file = File.open("exemple.txt")
puts file.read
```

En utilisant ces méthodes, nous pouvons facilement lire le contenu d'un fichier texte en Ruby. Mais qu'en est-il si nous voulons lire seulement une partie du fichier ? C'est là qu'entre en jeu la méthode `readline`. Cette méthode nous permet de lire une seule ligne à la fois du fichier. Voici un exemple :

```Ruby
file = File.open("exemple.txt")
puts file.readline
```

Cela ne lit que la première ligne du fichier. Nous pouvons également utiliser cette méthode à plusieurs reprises pour lire les lignes suivantes, jusqu'à ce que le fichier soit entièrement lu.

## Plongeons plus profondément

Il peut arriver que nous ayons besoin de lire un fichier texte très volumineux. Dans ce cas, il n'est pas pratique de lire tout le fichier en une fois. C'est pourquoi nous pouvons utiliser une boucle pour lire le contenu ligne par ligne. Voici un exemple :

```Ruby
file = File.open("exemple.txt")
file.each do |line|
  puts line
end
```

Cette boucle lit chaque ligne du fichier et la stocke dans la variable `line`, que nous pouvons ensuite afficher dans la console. Cela nous permet de parcourir tout le fichier sans avoir à charger tout son contenu en mémoire.

## Voir aussi

- Documentation Ruby sur la lecture de fichiers : https://ruby-doc.org/core-2.7.2/File.html
- Tutoriel vidéo sur la lecture de fichiers en Ruby : https://www.youtube.com/watch?v=DkbOvx5KP5Y
- Exemples de code pour la lecture de fichiers en Ruby : https://www.w3resource.com/ruby-exercises/file/index.php
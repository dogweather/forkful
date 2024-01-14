---
title:    "Ruby: Lecture d'un fichier texte"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Il existe de nombreuses raisons pour lesquelles vous pourriez avoir besoin de lire un fichier texte en utilisant Ruby. Cela peut être utile pour extraire des données d'un fichier de données, pour manipuler des données dans des formats spécifiques ou pour lire et traiter des fichiers de configuration.

## Comment faire

Il existe plusieurs façons de lire un fichier texte en utilisant Ruby. La méthode la plus simple consiste à utiliser la méthode `File.open` avec le chemin d'accès au fichier comme argument. Vous pouvez également spécifier les options de lecture telles que `r` pour lire le fichier en mode lecture seule ou `w` pour écrire dans le fichier.

Voici un exemple de code Ruby pour lire un fichier texte et afficher son contenu :

```Ruby
fichier = File.open("mon_fichier.txt", "r")

fichier.each_line do |line|
  puts line
end

fichier.close
```

Lorsque vous exécutez ce code, vous verrez chaque ligne du fichier affichée dans votre console. Vous pouvez également utiliser des méthodes telles que `read` ou `readlines` pour stocker le contenu du fichier dans une variable pour un traitement ultérieur.

## Plongée en profondeur

En plus de lire un fichier texte, Ruby offre également des méthodes pour écrire, supprimer et renommer des fichiers. Vous pouvez également utiliser des options de lecture telles que `b` pour les fichiers binaires ou spécifier l'encodage du fichier avec des options telles que `encoding: 'UTF-8'`.

Il est également important de noter que lors de la lecture d'un fichier, la méthode `each_line` renvoie chaque ligne du fichier sous forme de chaîne de caractères, y compris les caractères de saut de ligne. Si vous souhaitez supprimer ces caractères de saut de ligne, vous pouvez utiliser la méthode `chomp` pour les éliminer avant de traiter les données.

## Voir aussi

- [Documentation officielle de Ruby sur la lecture et l'écriture de fichiers](https://ruby-doc.org/core-3.0.0/File.html)
- [Tutoriel sur la manipulation de fichiers en Ruby](https://www.rubyguides.com/ruby-tutorial/io/)
- [Stack Overflow : Comment lire un fichier texte en Ruby ?](https://stackoverflow.com/questions/3682359/how-to-read-a-file-in-ruby)
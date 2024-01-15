---
title:                "Lecture d'un fichier texte"
html_title:           "Ruby: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation, vous vous demandez peut-être pourquoi vous devriez vous intéresser à lire un fichier texte. Eh bien, la lecture de fichiers texte est une tâche courante en programmation, surtout si vous travaillez avec des données ou des fichiers de configuration. Comprendre comment lire des fichiers texte peut vous aider à automatiser des tâches et à manipuler des données plus facilement.

## Comment faire

Pour lire un fichier texte en Ruby, il suffit d'utiliser la méthode `File.readlines` et de lui passer le nom du fichier en tant qu'argument. Cette méthode renverra un tableau contenant chaque ligne du fichier en tant qu'élément. Voyons un exemple :

```Ruby
file = File.readlines("mon_fichier.txt")

puts file # Affiche toutes les lignes du fichier
puts file[0] # Affiche la première ligne du fichier
```

Vous pouvez également parcourir le tableau et effectuer des opérations spécifiques sur chaque ligne, par exemple :

```Ruby
file.each do |line|
  puts line.capitalize # Affiche chaque ligne du fichier en majuscule
end
```

## Plongée en profondeur

Lorsque vous lisez un fichier texte en Ruby, il est important de comprendre le mode d'ouverture du fichier par défaut. Par défaut, la méthode `File.readlines` ouvre le fichier en mode lecture seule. Cela signifie que vous ne pourrez pas écrire dans le fichier ou le modifier. Si vous voulez effectuer des modifications dans le fichier, vous devrez l'ouvrir en mode lecture et écriture avec la méthode `File.open`.

De plus, il est important de noter que la méthode `File.readlines` renverra une erreur si le fichier n'existe pas ou si vous n'avez pas les permissions appropriées pour y accéder. Assurez-vous donc de gérer ces cas dans votre code.

## Voir aussi

- [Documentation officielle de Ruby sur la lecture de fichiers](https://ruby-doc.org/core-2.7.0/File.html#method-c-readlines)
- [Tutoriel sur la lecture et l'écriture de fichiers en Ruby](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- [Exemple de projet pratique utilisant la lecture de fichiers en Ruby](https://medium.com/tech-tajawal/file-manipulation-in-ruby-reading-from-a-text-file-4773f7c6bb9b)
---
title:                "Création d'un fichier temporaire"
html_title:           "Ruby: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Créer un fichier temporaire est une action couramment utilisée par les programmeurs Ruby. Cela consiste à créer un fichier qui sera utilisé temporairement pour stocker des données ou exécuter certaines actions. Les programmeurs le font souvent lorsqu'ils ont besoin d'écrire ou de lire des données sans avoir à utiliser un fichier permanent.

## Comment faire:

```Ruby
# Créer un fichier temporaire et écrire des données dedans
temp_file = File.open("example.txt", "w")
temp_file.puts("Ceci est un fichier temporaire")
temp_file.close

# Lire les données stockées dans le fichier temporaire
temp_file = File.open("example.txt", "r")
puts temp_file.read
temp_file.close
```

Output:
```
"Ceci est un fichier temporaire"
```

## Plongée en profondeur

Créer des fichiers temporaires est une pratique courante depuis de nombreuses années dans le monde de la programmation. Elle est particulièrement utile lorsque l'on travaille avec des données sensibles qui ne doivent pas être stockées en permanence. Les programmeurs ont également la possibilité d'utiliser des variables ou des structures de données en mémoire, mais cela peut causer des problèmes de performances lorsque les données sont volumineuses.

Heureusement, Ruby offre une solution simple et efficace pour créer des fichiers temporaires avec la méthode `File.open`. Il existe également des alternatives telles que la bibliothèque standard `Tempfile` ou des outils externes comme `TempfilePlus` qui proposent des fonctionnalités supplémentaires telles que la suppression automatique des fichiers temporaires après leur utilisation.

L'implémentation de la création de fichiers temporaires dans Ruby est basée sur la fonction `mkstemp` du système d'exploitation, qui crée un fichier avec une structure de nom aléatoire pour éviter les conflits avec d'autres fichiers.

## Voir aussi

- [La documentation officielle de Ruby sur la méthode File.open](https://ruby-doc.org/core-2.7.1/File.html#method-c-open)
- [La bibliothèque standard Tempfile](https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html)
- [La gemme TempfilePlus](https://rubygems.org/gems/tempfile_plus)
---
title:                "Lecture d'un fichier texte"
date:                  2024-01-20T17:55:07.293901-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Lire un fichier texte, c'est extraire son contenu pour l'utiliser dans nos programmes. On le fait pour traiter des données, pour configurer nos applications, ou pour lire des informations stockées.

## How to:

Lire un fichier en Ruby est simple. Voici un exemple basique :

```Ruby
# Lire le contenu intégral d'un fichier
content = File.read('exemple.txt')
puts content
```

Et si le fichier est gros, on le lit ligne par ligne pour économiser la mémoire :

```Ruby
# Lire un fichier ligne par ligne
File.foreach('exemple.txt') do |line|
  puts line
end
```

Sample output :

```
Ceci est la première ligne du fichier.
Voici la seconde ligne.
Et finalement, voici la troisième ligne.
```

Pour manipuler les fichiers plus finement (comme spécifier l'encodage), on utilise une forme plus détaillée :

```Ruby
# Ouvrir un fichier avec un encodage spécifique
File.open('exemple.txt', "r:UTF-8") do |file|
  file.each_line do |line|
    puts line
  end
end
```

## Deep Dive

Historiquement, la lecture de fichiers en Ruby a beaucoup évolué. Avant, on utilisait des méthodes moins intuitives, mais depuis Ruby 1.0, les choses se sont simplifiées.

Il y a des alternatives à `File.read` et `File.foreach`. Par exemple :

- `IO.readlines('exemple.txt')` pour obtenir un tableau de toutes les lignes.
- `File.open('exemple.txt')` suivi de `.readline` ou `.readlines` pour plus de contrôle.

Côté implémentation, Ruby gère les fichiers à travers la classe `File` qui hérite de `IO`. Cette conception permet de traiter les fichiers comme des flux d'entrée/sortie, donnant plus de flexibilité. L'encodage est aussi géré avec soin – important dans un contexte multilingue.

## See Also

Pour en apprendre plus :

- Document officiel Ruby sur la classe File : [Ruby-Doc.org File Class](https://www.ruby-doc.org/core/File.html)
- Didacticiel sur la manipulation de fichiers en Ruby : [RubyGuides - File IO](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- Apprendre l'API IO de Ruby pour les opérations d'entrée/sortie : [IO Class](https://www.ruby-doc.org/core/IO.html)

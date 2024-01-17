---
title:                "Ecrire un fichier texte"
html_title:           "Ruby: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & pourquoi?
Écrire un fichier texte en programmation consiste à créer un fichier contenant du texte, qui peut être lu et modifié par un programme informatique. Les programmeurs utilisent cette méthode pour stocker des données comme des paramètres, des configurations, ou pour générer des rapports à partir de leur code.

## Comment:
Exemples de code et sortie:
```
# Pour écrire dans un fichier texte, il faut utiliser la méthode open avec l'argument "w" pour spécifier l'écriture.
fichier = File.open("nom_du_fichier.txt", "w") 
# Ensuite, on peut utiliser la méthode puts pour écrire du texte dans le fichier.
fichier.puts "Bonjour le monde!" 
# N'oubliez pas de fermer le fichier une fois que vous avez fini d'écrire.
fichier.close 
```

## Plongée en profondeur:
Historiquement, les programmeurs ont dû écrire manuellement des fichiers texte pour stocker des données. Cependant, avec l'augmentation de la puissance de traitement des ordinateurs, des méthodes plus efficaces sont apparues, comme les bases de données. Cependant, écrire un fichier texte reste une méthode simple et largement utilisée pour stocker des données. Implémenter cette fonctionnalité nécessite des connaissances de base en programmation et des compétences en manipulation de fichiers.

## Voir aussi:
- [Ruby Documentation officielle sur File](https://ruby-doc.org/core-2.7.1/File.html)
- [Les bases de la manipulation de fichiers en Ruby](https://www.theodinproject.com/courses/ruby-programming/lessons/file-i-o-and-serialization)
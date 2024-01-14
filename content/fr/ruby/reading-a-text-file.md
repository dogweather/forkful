---
title:                "Ruby: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi
Lire des fichiers texte est une tâche fondamentale pour tout programmeur Ruby. Cela permet de manipuler facilement des données externes et d'en extraire des informations utiles. Que vous soyez un débutant ou un programmeur expérimenté, il est nécessaire de savoir comment lire des fichiers texte en utilisant Ruby.

## Comment faire
Pour lire un fichier texte en Ruby, vous pouvez utiliser la méthode "File.open". Voici un exemple de code:

```Ruby
File.open("mon_fichier.txt").each do |ligne|
  puts ligne
end
```

Ce code va ouvrir le fichier "mon_fichier.txt" et afficher chaque ligne à l'écran. Vous pouvez également utiliser la méthode "readlines" pour stocker chaque ligne du fichier dans un tableau. Par exemple:

```Ruby
lignes = File.readlines("mon_fichier.txt")
puts lignes
```

L'utilisation de ces méthodes vous permet de lire des fichiers texte de différentes manières en fonction de vos besoins.

## Profondeur de plongée
Il est important de noter que les fichiers texte peuvent contenir des données dans différents formats, tels que CSV, JSON ou XML. Vous devez donc être en mesure de lire et de manipuler ces formats en utilisant Ruby. Par exemple, pour lire un fichier CSV, vous pouvez utiliser la gemme "csv". Voici un exemple de code:

```Ruby
require 'csv'

CSV.foreach("mon_fichier.csv") do |ligne|
  puts ligne
end
```

De plus, vous devriez également être conscient des erreurs courantes lors de la lecture de fichiers texte, telles que les fichiers manquants ou les erreurs de formatage. Il est important de gérer ces erreurs de manière appropriée afin de garantir que votre programme fonctionne correctement.

# Voir aussi
- [Documentation officielle Ruby sur la lecture de fichiers](https://ruby-doc.org/core-2.5.0/File.html)
- [Gemme CSV pour la manipulation de fichiers CSV en Ruby](https://github.com/ruby/csv)
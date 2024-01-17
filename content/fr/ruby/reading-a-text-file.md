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

## Qu'est-ce que c'est & Pourquoi? 
La lecture d'un fichier de texte est l'action de parcourir le contenu d'un fichier contenant du texte en utilisant un programme informatique. Les programmeurs le font souvent lorsqu'ils doivent traiter de grandes quantités de données stockées dans des fichiers sous forme de texte.

## Comment faire:
Voici un exemple de code en Ruby pour lire un fichier de texte et afficher son contenu: 
```ruby
File.foreach("fichier.txt") do |line|
  puts line
end
```
Voici un exemple de sortie pour un fichier contenant du texte:
```
Ceci est une ligne de texte.
Voici une autre ligne.
Et une dernière pour la route.
```

## Plongée en profondeur:
Lecture de fichiers texte remonte aux premiers jours de la programmation informatique lorsque les ordinateurs stockaient des données sous forme de texte dans des fichiers. De nos jours, il existe également d'autres manières de stocker et de lire des données telles que les bases de données ou les fichiers binaires. Cependant, la lecture de fichiers texte reste utile pour traiter des données simples et peut être réalisée avec une variété de langages de programmation.

## Voir aussi:
- [Documentation officielle de Ruby sur la lecture de fichiers](https://ruby-doc.org/core-3.0.1/IO.html)
- [Un tutoriel en français sur la lecture de fichiers en Ruby](https://zetcode.com/lang/rubytutorial/files/)
- [Un article sur les différentes façons de stocker des données en informatique](https://www.vertabelo.com/blog/what-are-the-different-types-of-data-storage/)
---
title:                "Lecture d'un fichier texte"
html_title:           "Gleam: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Lire un fichier texte est une tâche courante dans la programmation. Cela implique de parcourir un fichier pour en extraire les informations qu'il contient. Les programmeurs le font pour traiter des données, analyser des fichiers ou pour d'autres besoins spécifiques.

## Comment faire: 

Voici un exemple de code en ```Gleam``` montrant comment lire un fichier texte ligne par ligne et afficher chaque ligne:

```
import ellesmera/file

let file = ellesmera/file.open("fichier.txt")
file |>  ellesmera/file.read_line
|> enum.iter(print)
```

Output:
```
Bonjour!
Comment ça va?
Je suis un fichier texte.
```

## Plongée en profondeur:
 
Lire des fichiers texte est un processus de longue date et a évolué au fil du temps. Il existe plusieurs alternatives telles que le CSV (Comma-Separated Values) ou le JSON (JavaScript Object Notation) pour stocker et échanger des données. Dans Gleam, la prise en charge native du format de données standard [They](https://github.com/gleam-lang/gleam) rend la lecture de fichiers texte plus pratique. Les fichiers texte sont généralement lus en utilisant des routines d'entrée/sortie telles que ```open``` et ```read``` qui sont mises en œuvre avec des manipulations de fichiers bas niveau.

## Voir aussi:

Si vous souhaitez en savoir plus sur la lecture de fichiers texte et les alternatives, voici quelques liens utiles:
- [Documentation Gleam sur la bibliothèque standarde pour les fichiers](https://gleam.run/modules/ellesmera/file.html#open)
- [Wikipedia page sur les formats de fichier de données](https://en.wikipedia.org/wiki/Data_format)
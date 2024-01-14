---
title:    "Gleam: Lecture d'un fichier texte"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi
La lecture de fichiers texte est une compétence essentielle pour tout programmeur, qu'il s'agisse de manipuler des données ou de créer des applications. Cela peut sembler simple, mais comprendre comment lire correctement un fichier texte peut être très utile dans une variété de projets.

## Comment faire
Voici un exemple de code en Gleam pour lire un fichier texte et le stocker dans une variable :

```Gleam
let texte = File.read("mon_fichier.txt") } 
```

Le résultat de cette opération sera le contenu du fichier texte stocké dans la variable "texte". Pour afficher ce contenu, nous pouvons utiliser la fonction "io.print()" :

```Gleam
io.print(texte)
```

Le résultat sera l'affichage du contenu du fichier texte dans la console. Vous pouvez également utiliser la fonction "List.iter()" pour parcourir le contenu du fichier ligne par ligne :

```Gleam
List.iter(texte, fun(line) { 
  io.print(line) 
})
```

## Plongée en profondeur
Outre la lecture simple d'un fichier texte, il est possible de manipuler le contenu en utilisant les fonctionnalités de traitement de chaînes de caractères en Gleam. Par exemple, vous pouvez utiliser la fonction "String.split()" pour découper le texte en différentes parties en fonction d'un délimiteur spécifique. Vous pouvez également utiliser la fonction "String.trim()" pour supprimer les espaces vides avant et après le contenu.

De plus, il existe des bibliothèques tierces en Gleam qui offrent des fonctionnalités avancées pour la lecture de fichiers texte, telles que la prise en charge de différents encodages ou la lecture de grandes quantités de données.

## Voir aussi
- [Documentation de Gleam sur la lecture de fichiers](https://gleam.run/book/std/file.html)
- [Exemples de code pour la lecture de fichiers en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/io/file_read.gleam)
- [Bibliothèque tierce pour la lecture de fichiers en Gleam](https://github.com/sasa1977/file)
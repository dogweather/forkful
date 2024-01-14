---
title:    "Gleam: Lecture d'un fichier texte"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi lire un fichier texte en programmation?

Lire un fichier texte en programmation est une tâche courante et importante pour de nombreuses raisons. Que ce soit pour traiter des données, extraire des informations ou générer des rapports, la manipulation de fichiers texte est une compétence essentielle à maîtriser pour tout développeur.

## Comment faire?

Voici un exemple de code en Gleam pour lire un fichier texte et afficher son contenu:

```Gleam
let content = File.read("mon_fichier.txt")
println(content)
```

Le code ci-dessus utilise la fonction `read` de la bibliothèque standard de Gleam pour lire le fichier "mon_fichier.txt" et stocker son contenu dans la variable `content`. Ensuite, la fonction `println` est utilisée pour afficher le contenu à l'écran.

## Plongée en profondeur

La lecture d'un fichier texte peut sembler simple, mais il existe plusieurs façons de le faire en fonction de vos besoins. Par exemple, vous pouvez spécifier un encodage spécifique si votre fichier contient des caractères non ASCII, ou vous pouvez utiliser des méthodes de lecture plus efficaces pour les gros fichiers.

De plus, vous pouvez également écrire dans un fichier texte en utilisant la fonction `write` de la bibliothèque standard de Gleam, ce qui peut être utile lorsqu'il s'agit de générer des rapports ou de sauvegarder des données.

## Voir aussi

- La documentation de la fonction `read` de la bibliothèque standard de Gleam : https://gleam.run/documentation/std-lib-file.html#read
- La documentation de la fonction `write` de la bibliothèque standard de Gleam : https://gleam.run/documentation/std-lib-file.html#write
- Un tutoriel sur la manipulation de fichiers texte en Gleam : https://dev.to/kbeyazli/a-gentle-introduction-to-file-i-o-in-gleam-programming-15fc
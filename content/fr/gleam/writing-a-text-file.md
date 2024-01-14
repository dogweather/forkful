---
title:                "Gleam: Rédiger un fichier texte"
simple_title:         "Rédiger un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut sembler banal d'écrire un simple fichier texte, mais en réalité, c'est une tâche très utile pour les programmeurs en Gleam. Les fichiers textes sont une forme commune de stockage de données et les manipuler est un moyen efficace de gérer des informations dans vos programmes.

## Comment faire

Pour écrire un fichier texte en Gleam, utilisez la fonction `File.write` en passant le chemin du fichier et le contenu que vous souhaitez écrire. Par exemple, si vous voulez écrire "Bonjour le monde !" dans un fichier appelé "mon_fichier.txt", vous pouvez le faire comme suit:

```Gleam
File.write("mon_fichier.txt", "Bonjour le monde !")
```

Si le fichier n'existe pas encore, il sera créé automatiquement. Si le fichier existe déjà, son contenu sera remplacé par le nouveau contenu que vous avez fourni.

## Plongée en profondeur

Lorsque vous écrivez un fichier texte en Gleam, vous pouvez également spécifier le mode d'écriture à utiliser. Par défaut, Gleam utilise le mode d'écriture "w" qui remplace le contenu du fichier à chaque appel de `File.write`. Vous pouvez également utiliser le mode d'écriture "a", qui ajoutera le contenu à la fin du fichier au lieu de le remplacer.

En outre, vous pouvez également spécifier la langue d'écriture en utilisant le paramètre `encoding`. Par défaut, Gleam utilise l'encodage de caractères UTF-8, mais vous pouvez choisir d'autres encodages tels que UTF-16 ou ASCII si nécessaire.

## Voir aussi

- La documentation officielle de la fonction `File.write`: https://gleam.run/std/file#write
- Un tutoriel sur les fichiers en Gleam: https://gleam.run/book/tutorials/files

Merci de lire cet article sur l'écriture de fichiers texte en Gleam ! Nous espérons que cela vous a été utile dans votre apprentissage de ce langage de programmation fonctionnelle moderne. N'oubliez pas d'explorer davantage les nombreuses possibilités qu'offre Gleam pour gérer et manipuler les fichiers. Bonne programmation !
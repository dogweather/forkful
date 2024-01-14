---
title:                "Elm: Création d'un fichier temporaire"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer des fichiers temporaires en Elm?

Créer des fichiers temporaires peut sembler être une tâche inutile, mais elle peut en fait être très utile lors de la programmation en Elm. Que ce soit pour stocker des données temporaires ou pour éviter de surcharger votre système de fichiers avec des fichiers inutiles, apprendre à créer des fichiers temporaires en Elm peut vous être très pratique.

## Comment créer des fichiers temporaires en Elm

Il existe plusieurs façons de créer des fichiers temporaires en Elm, mais voici l'une des méthodes les plus simples. Tout d'abord, nous devons importer la bibliothèque `File` qui nous permettra de travailler avec des fichiers en Elm. Ensuite, nous pouvons utiliser la fonction `createTemp` pour créer un fichier temporaire avec un nom aléatoire.

```Elm
import File
file : File
file = File.createTemp ()
```

Cela créera un fichier temporaire dans le dossier de votre projet avec un nom aléatoire, tel que "temp1234". Vous pouvez alors utiliser ce fichier pour stocker des données ou effectuer d'autres opérations.

## Plongée en profondeur

Bien que la méthode ci-dessus soit simple et pratique, il est important de comprendre le processus de création d'un fichier temporaire en Elm. Lorsque nous utilisons la fonction `createTemp`, un nom aléatoire est généré et le fichier est créé dans le dossier de notre projet. Cependant, le fichier n'existe pas réellement tant que nous n'avons pas écrit de données à l'intérieur. Cela signifie que si vous essayez de lire le contenu du fichier avant d'y avoir écrit, vous obtiendrez une erreur.

Il est également important de noter que les fichiers temporaires créés de cette manière seront automatiquement supprimés lors de la fermeture de votre application ou lorsque le Garbage Collector sera appelé.

## Voir aussi

- Documentation sur la bibliothèque `File`: https://package.elm-lang.org/packages/elm/core/latest/File
- Exemples de code pour créer des fichiers temporaires: https://github.com/elm/random/blob/master/examples/temp_file.elm
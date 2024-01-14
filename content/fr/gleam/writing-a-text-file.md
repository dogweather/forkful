---
title:    "Gleam: Écrire un fichier texte"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en Gleam?

Écrire un fichier texte peut sembler une tâche simple, mais en utilisant le langage de programmation fonctionnelle Gleam, vous obtenez une approche élégante pour manipuler les fichiers et les données à l'intérieur. Cela peut être utile lorsque vous travaillez avec de gros volumes de données ou que vous avez besoin d'effectuer des opérations complexes sur votre fichier texte.

## Comment faire?

Voici un exemple de code pour créer un fichier texte avec Gleam:

```Gleam
import gleam/io

// Créer un fichier texte avec une ligne
let file = io.write_text_file("mon_fichier.txt", "Ceci est un exemple de ligne.")

// Ajouter une nouvelle ligne au fichier
file = io.write_text_file(file, "Et ceci est une autre ligne.")

// Fermer le fichier
file = io.close_text_file(file)
```

Voici le contenu de "mon_fichier.txt" après l'exécution de ce code:

Ceci est un exemple de ligne.
Et ceci est une autre ligne.

Comme vous pouvez le voir, chaque fois que vous ajoutez une nouvelle ligne à votre fichier, celle-ci est ajoutée à la fin du fichier existant. Vous pouvez également utiliser les fonctions de lecture de fichier de Gleam pour lire et manipuler le contenu de votre fichier texte.

## Plongée en profondeur

Gleam utilise le concept de gestionnaire de ressources afin de gérer automatiquement l'ouverture et la fermeture des fichiers. Cela vous permet de vous concentrer sur la manipulation des données à l'intérieur de votre fichier plutôt que de vous soucier de la gestion des fichiers. De plus, Gleam offre plusieurs fonctions et options pour personnaliser la façon dont vous créez et manipulez vos fichiers texte.

## Voir aussi

- La documentation officielle de Gleam sur les fichiers: [https://gleam.run/articles/files/](https://gleam.run/articles/files/)
- Un tutoriel sur la manipulation des fichiers en Gleam: [https://medium.com/koalTea/how-to-manipulate-files-in-gleam-1c01a3caef5e](https://medium.com/koalTea/how-to-manipulate-files-in-gleam-1c01a3caef5e)
- Un autre article sur l'utilisation de Gleam pour créer des fichiers CSV: [https://thoughtbot.com/blog/using-gleam-to-generate-a-simple-csv-file](https://thoughtbot.com/blog/using-gleam-to-generate-a-simple-csv-file)
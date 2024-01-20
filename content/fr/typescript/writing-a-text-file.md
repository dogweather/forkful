---
title:                "Écrire un fichier texte"
html_title:           "TypeScript: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
L'écriture d'un fichier texte peut sembler une tâche simple pour les programmeurs, mais elle est essentielle pour de nombreuses applications. Cela consiste à créer ou à modifier un fichier texte, qui est un fichier contenant uniquement du texte brut sans aucun formatage ou graphique. Les programmeurs utilisent souvent cette technique pour créer des fichiers de données ou de configuration, ou pour enregistrer des informations saisies par l'utilisateur.

## Comment faire:
Voici un exemple en TypeScript pour écrire un fichier texte:

```
import { writeFileSync } from "fs";

// Création d'un fichier texte nommé "noms.txt"
writeFileSync("noms.txt", "John" + "\n" + "Sarah" + "\n" + "Tom");

// Output: Un fichier texte contenant les noms John, Sarah et Tom
```

Pour ajouter du contenu à un fichier existant, vous pouvez utiliser la méthode `appendFileSync` à la place de `writeFileSync`.

```
import { appendFileSync } from "fs";

// Ajout d'un nouveau nom à notre fichier "noms.txt"
appendFileSync("noms.txt", "\n" + "Amy");

// Output: Le fichier texte contient maintenant les noms John, Sarah, Tom et Amy
```

## Plongée en profondeur:
L'écriture de fichiers texte remonte aux débuts de la programmation informatique. Autrefois, les programmes stockaient principalement leurs données dans des fichiers texte avant l'avènement des bases de données. Il existe également d'autres alternatives pour stocker des données, telles que les fichiers CSV ou les fichiers JSON, mais les fichiers texte restent encore très populaires en raison de leur simplicité et de leur compatibilité avec la plupart des langages de programmation.

L'implémentation de l'écriture de fichiers texte varie en fonction du langage de programmation utilisé, mais les étapes de base restent les mêmes: création d'un fichier, ajout ou modification de contenu, puis enregistrement du fichier sur le disque.

## Voir aussi:
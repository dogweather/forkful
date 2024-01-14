---
title:    "TypeScript: Créer un fichier temporaire"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en TypeScript

Lors de la programmation en TypeScript, il peut être utile de créer des fichiers temporaires. Ces fichiers sont créés pour stocker temporairement des données ou des informations et sont supprimés une fois qu'ils ne sont plus nécessaires. Dans cet article, nous allons explorer pourquoi il peut être nécessaire de créer un fichier temporaire en TypeScript et comment le faire.

## Comment créer un fichier temporaire en TypeScript
Pour créer un fichier temporaire en TypeScript, nous utiliserons la bibliothèque "fs" de Node.js. Cette bibliothèque fournit des fonctions pour gérer les fichiers et les dossiers. Nous utiliserons la fonction "mkdtempSync()" pour créer un dossier temporaire et la fonction "writeFileSync()" pour écrire nos données dans ce dossier.

```TypeScript
import * as fs from 'fs';

// Création du dossier temporaire
const tempDir = fs.mkdtempSync('/tmp/');

// Écriture des données dans le dossier temporaire
fs.writeFileSync(`${tempDir}/exemple.txt`, 'Ceci est un exemple de données à stocker dans un fichier temporaire.');

// Récupération des données du fichier temporaire
const data = fs.readFileSync(`${tempDir}/exemple.txt`);

// Affichage des données
console.log(data.toString()); // Sortie: "Ceci est un exemple de données à stocker dans un fichier temporaire."
```

## Approfondissement sur la création de fichiers temporaires en TypeScript
Dans l'exemple précédent, nous avons utilisé deux fonctions de la bibliothèque "fs". La fonction "mkdtempSync()" prend en paramètres un préfixe pour le nom du dossier temporaire et renvoie le chemin du dossier créé. La fonction "writeFileSync()" prend en paramètres le chemin du fichier à écrire et les données à y inscrire.

Il est également possible de spécifier des options pour le dossier temporaire dans la fonction "mkdtempSync()". Par exemple, nous pouvons spécifier un codage pour les noms de fichiers générés ou encore une extension pour les fichiers créés.

Il est important de noter qu'il est conseillé de supprimer manuellement le dossier temporaire une fois que vous avez fini d'utiliser le fichier temporaire. Cela peut être fait en utilisant la fonction "unlinkSync()" de la bibliothèque "fs".

## Voir aussi
- [Documentation de la bibliothèque fs de Node.js](https://nodejs.org/api/fs.html)
- [Tutoriel sur les fichiers temporaires en Node.js](https://www.digitalocean.com/community/tutorials/how-to-use-the-tmp-folder-in-node-js)
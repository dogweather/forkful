---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Lecture d'un fichier texte, c'est simplement d'extraire du contenu texte à partir de fichier. Les programmeurs le font pour accéder aux données stockées, pour analyser ou modifier les informations.

## Comment faire:

Dans TypeScript, il existe diverses méthodes pour lire un fichier texte. La plus couramment utilisée est le module `fs` (fichier système) de Node.js. Voici comment le faire:

```TypeScript
import fs from 'fs/promises';

async function lireFichierTexte(fichier: string) {
    try {
        const données = await fs.readFile(fichier, 'utf-8');
        console.log(données);
    } catch (erreur) {
        console.error(`Erreur lors de la lecture du fichier: ${erreur}`);
    }
}

lireFichierTexte('monFichier.txt');
```

## Plongée en profondeur:

Depuis ses débuts, la lecture des fichiers texte en JavaScript (et donc en TypeScript) a évolué. Au départ, cela se faisait généralement côté serveur via Node.js et son module `fs`. Aujourd'hui, nous utilisons toujours la même approche mais dans une version modernisée et plus sécurisée avec `fs/promises`.

Il existe des alternatives, comme l'API `fetch` pour les environnements front-end. Cependant, elle présente des limitations en termes de permissions.

Dans l'implémentation ci-dessus, nous utilisons une fonction asynchrone pour attendre que `fs.readFile()` soit résolue. Cela permet de lire le fichier texte sans bloquer le reste du code. 

## Voir aussi:

Pour plus d'informations sur la manipulation de fichiers en JavaScript/TypeScript, voici quelques liens utiles:

- [Documentation officielle de Node.js sur fs/promises](https://nodejs.org/api/fs.html#fs_fs_promises_api)
- [Article Mozilla sur l'API Fetch](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API)
- [Guide de survie à la programmation asynchrone en JavaScript/TypeScript](https://www.freecodecamp.org/news/async-programming-in-typescript/)
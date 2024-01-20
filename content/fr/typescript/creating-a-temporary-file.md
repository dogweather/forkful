---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi ?

La création d'un fichier temporaire est la mise en place d'un stockage de données à court terme dans le système de fichiers. Les programmeurs le font pour traiter de grosses données en morceaux gérables ou pour stocker des informations temporaires.

## Comment faire :

Créer un fichier temporaire en TypeScript peut se faire grâce à des packages comme `tmp-promise`. Voici un exemple :

```TypeScript
import { file } from 'tmp-promise';

(async () => {
  const { path, cleanup } = await file();
  console.log('Path temporaire du fichier:', path);
  cleanup(); // Nettoyer le fichier après utilisation
})();
```

Quand vous exécutez ce code, vous verrez une sortie qui ressemble à cela :

```TypeScript
Path temporaire du fichier: /tmp/tmp-123xyz.txt
```

## Plongée profonde :

Historiquement, les fichiers temporaires étaient utilisés pour le stockage à court terme en raison des limites de mémoire des ordinateurs. Aujourd'hui, ils sont également utilisés pour gérer de gros flux de données ou pour des activités où la persistance des données n'est pas nécessaire.

Les alternatives à l'utilisation des fichiers temporaires incluent le stockage en mémoire avec des structures de données comme les tableaux ou les objets. Cependant, ces méthodes peuvent rapidement utiliser toute la mémoire disponible si les données sont trop importantes.

Lors de la création d'un fichier temporaire en TypeScript, vous pouvez définir des options supplémentaires, comme l'extension de fichier ou le préfixe. Mais, le facteur essentiel est de ne pas oublier le nettoyage du fichier temporaire après l'utilisation.

## Voir aussi :

1. [Introduction à Node.js](http://nodejs.org/) : Pour en savoir plus sur la plateforme qui rend possible la programmation TypeScript côté serveur.
2. [Documentation de tmp-promise](https://github.com/benjamingr/tmp-promise) : Pour des informations détaillées sur les méthodes utilisées dans cet exemple.
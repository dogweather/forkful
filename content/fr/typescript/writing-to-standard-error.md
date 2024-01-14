---
title:                "TypeScript: Écrire vers l'erreur standard"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi écrire vers l'erreur standard en TypeScript ?

Ecrire vers l'erreur standard, ou "stdout" en anglais, est une pratique courante dans le monde de la programmation. Cela permet d'afficher des messages d'erreur ou des informations de débogage directement dans la console de l'utilisateur. En utilisant TypeScript, cette pratique peut être encore plus efficace grâce à la forte typage et à la vérification des types statique, qui permettent de détecter plus facilement les erreurs de programmation.

## Comment faire ?

Pour commencer, il faut importer le module "process" dans votre fichier TypeScript :

```TypeScript
import * as process from 'process';
```

Ensuite, vous pouvez utiliser la fonction "process.stderr.write" pour écrire un message vers l'erreur standard. Voici un exemple simple :

```TypeScript 
process.stderr.write("Une erreur est survenue !");
```

Vous pouvez également utiliser des variables dans votre message, en les encadrant par des backticks :

```TypeScript
const count = 5;
process.stderr.write(`Il y a ${count} erreurs.`);
```

Le résultat obtenu dans la console sera : "Une erreur est survenue ! Il y a 5 erreurs.".

## Zoom sur l'écriture vers l'erreur standard

En utilisant la fonction "process.stderr.write", vous avez la possibilité d'ajouter des options supplémentaires pour personnaliser l'affichage de vos messages. Voici les options disponibles :

- "encoding" : spécifie l'encodage des données à écrire. Par défaut, c'est l'encodage du terminal qui est utilisé.
- "fd" : l'identifiant du fichier dans lequel écrire. Par défaut, c'est l'erreur standard.
- "allowHalfOpen" : si vrai, la file d'attente d'écriture ne se ferme pas automatiquement après l'écriture du message. Par défaut, c'est faux.
- "mode" : si "allowHalfOpen" est vrai, mode spécifie le mode de fichier à utiliser lors de l'ouverture automatique du fichier. Par défaut, c'est "0o666" en écriture.

## Voir aussi

- [Documentation officielle de process module en Node.js (en anglais)](https://nodejs.org/api/process.html)
- [Documentation officielle de TypeScript (en français)](https://www.typescriptlang.org/docs/)
- [Tutoriel sur les fonctions en TypeScript (en français)](https://www.tutorialsteacher.com/typescript/typescript-functions)
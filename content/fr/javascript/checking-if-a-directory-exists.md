---
title:    "Javascript: Vérifier si un répertoire existe"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

La vérification de l'existence d'un répertoire est une étape cruciale dans la programmation. Cela permet de s'assurer que le chemin d'accès que l'on souhaite utiliser est valide et que l'on pourra accéder aux fichiers ou dossiers nécessaires sans rencontrer d'erreurs.

## Comment faire

Pour vérifier si un répertoire existe en Javascript, on peut utiliser la méthode `fs.existsSync()` de Node.js. Cette méthode prend en paramètre le chemin d'accès au répertoire que l'on souhaite vérifier et retourne un booléen indiquant si le répertoire existe ou non.

```Javascript
const fs = require('fs');

// Vérifier si le répertoire 'mesFichiers' existe
if (fs.existsSync('mesFichiers')) {
    console.log('Le répertoire existe.');
} else {
    console.log('Le répertoire n\'existe pas.');
}
```
Output : Le répertoire existe.

## Deep Dive

Pour comprendre comment fonctionne cette méthode, il est important de connaître d'abord le module `fs` de Node.js. Ce module est utilisé pour effectuer des opérations liées au système de fichiers. Il contient des méthodes pour créer, supprimer, déplacer et vérifier l'existence de fichiers et de répertoires.

La méthode `fs.existsSync()` est synchronisée, ce qui signifie qu'elle bloque l'exécution du code jusqu'à ce que la vérification soit terminée. Si vous préférez une méthode asynchrone, vous pouvez utiliser `fs.exists()` qui prend en paramètre une callback pour traiter le résultat de la vérification.

Il est également important de noter que la vérification de l'existence d'un répertoire ne garantit pas que l'on pourra accéder aux fichiers ou dossiers à l'intérieur de celui-ci. Il peut y avoir des restrictions d'accès ou des permissions qui empêcheront l'accès aux fichiers même si le répertoire existe.

## Voir aussi

- [Documentation du module fs de Node.js](https://nodejs.org/api/fs.html)
- [Vérification de l'existence d'un dossier en Javascript avec fs.existsSync()](https://www.tutorialsteacher.com/nodejs/nodejs-check-file-exists)
- [Gestion des erreurs et des exceptions en Javascript - nettoyer le chemin d'accès](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Exceptions/Essayer_catch_finally)
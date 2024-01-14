---
title:                "Javascript: Écrire un fichier texte"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte en JavaScript?

Écrire un fichier texte en JavaScript est un moyen rapide et simple de stocker des informations. Cela peut être utile pour enregistrer des données utilisateur, des journaux de l'application ou des paramètres de configuration. Cela permet également de partager facilement des informations avec d'autres développeurs ou utilisateurs.

## Comment écrire un fichier texte en JavaScript?

Pour écrire un fichier texte en JavaScript, vous aurez besoin d'importer le module "fs". Ensuite, vous devrez utiliser la méthode "writeFileSync()" pour écrire dans le fichier. Voici un exemple de code pour créer un fichier texte nommé "texte.txt" et y écrire le contenu "Bonjour le monde!".

```javascript
const fs = require('fs');

fs.writeFileSync('texte.txt', 'Bonjour le monde!');
```

Si vous voulez ajouter du contenu à un fichier existant, vous pouvez utiliser la méthode "appendFileSync()". Voici un exemple de code pour ajouter du contenu au fichier "texte.txt" sans écraser son contenu précédent.

```javascript
fs.appendFileSync('texte.txt', 'Comment ça va?');
```

## Plongée en profondeur

Il est important de noter que le contenu écrit avec les méthodes "writeFileSync()" et "appendFileSync()" sera écrasé si vous réutilisez ces méthodes avec le même nom de fichier. Si vous voulez éviter cela, vous pouvez utiliser la méthode "writeFile()" qui prend un troisième argument, une fonction de rappel. Cette fonction sera appelée après l'écriture du fichier et vous pourrez y gérer les erreurs éventuelles.

```javascript
fs.writeFile('texte.txt', 'Salut tout le monde!', (err) => {
  if (err) throw err;
  console.log('Le fichier a été écrit avec succès!');
});
```

Vous pouvez également spécifier l'encodage du fichier en ajoutant un quatrième argument, mais par défaut cela est défini sur "utf8". Si vous voulez utiliser un autre encodage, vous devez spécifier "utf8" comme troisième argument.

## Voir aussi

Il existe de nombreuses autres méthodes pour écrire des fichiers en JavaScript, vous pouvez les découvrir dans la documentation officielle de Node.js sur le module "fs".

[Documentation Node.js - module "fs"](https://nodejs.org/api/fs.html)

Vous pouvez également apprendre à lire des fichiers texte en JavaScript en consultant cet article sur mon blog.

[Comment lire un fichier texte en JavaScript?](https://monblogdev.com/comment-lire-un-fichier-texte-en-javascript/)
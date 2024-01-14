---
title:    "Javascript: Écriture d'un fichier texte"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

Il est important pour un programmeur d'apprendre à écrire un fichier texte en Javascript. Les fichiers texte sont utilisés pour stocker des données et peuvent être manipulés avec des fonctions Javascript, ce qui peut être utile pour de nombreux projets.

## Comment faire

Pour écrire un fichier texte en Javascript, nous allons utiliser l'objet "fs" qui fait partie du noyau de Node.js. Il contient des fonctions pour lire, écrire et modifier les fichiers. Voici un exemple de code pour créer et écrire dans un fichier texte :

```Javascript
const fs = require('fs');
fs.writeFile('nom_du_fichier.txt', 'Contenu du fichier', (err) => {
  if (err) {
    console.log(err);
  } else {
    console.log('Le fichier a été créé et écrit avec succès.');
  }
});
```

La fonction `writeFile` prend trois arguments : le nom du fichier, le contenu à écrire et une fonction de rappel. Si une erreur se produit pendant l'écriture, elle sera affichée dans la console. Sinon, un message de succès sera affiché.

## Plongée en profondeur

Il est également possible de spécifier l'encodage du fichier lors de l'écriture en utilisant le troisième argument de la fonction `writeFile`. Par défaut, l'encodage est défini sur 'utf-8'. Vous pouvez également utiliser la fonction `appendFile` pour ajouter du contenu à un fichier existant plutôt que de le remplacer entièrement.

Il est important de noter que les informations écrites dans un fichier texte seront stockées sous forme de chaîne de caractères. Si vous souhaitez stocker des données plus complexes, comme des objets Javascript, vous devrez d'abord les convertir en JSON à l'aide de la fonction `JSON.stringify`.

## Voir aussi

Pour plus d'informations sur l'écriture de fichiers en Javascript, consultez ces liens utiles :

- [Documentation officielle de Node.js](https://nodejs.org/api/fs.html)
- [Tutoriel pour écrire un fichier avec fs en Node.js](https://www.digitalocean.com/community/tutorials/how-to-write-to-a-file-in-node-js)
- [Guide complet pour la création et l'écriture de fichiers en Javascript](https://www.freecodecamp.org/news/node-js-tutorial-create-and-write-files-fstream-npm-efd8c1dc3d6a/)
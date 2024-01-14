---
title:    "Javascript: Lecture d'un fichier texte"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

Lecture de fichiers texte en Javascript : pourquoi s'engager dans cette tâche ? 

Lire un fichier texte est une activité essentielle pour tout développeur. Que ce soit pour récupérer des données, traiter des informations ou simplement pour tester votre code, la capacité de lire un fichier est un élément vital de la programmation. Dans cet article, nous allons expliquer comment lire un fichier texte en utilisant Javascript, afin que vous puissiez maîtriser cette compétence précieuse dès maintenant.

## Comment faire

La première étape pour lire un fichier texte en Javascript est de définir le fichier que vous souhaitez lire. Cela peut être un fichier présent sur votre ordinateur ou en ligne via une URL. Pour cet exemple, nous allons utiliser un fichier texte local nommé "test.txt". Vous pouvez utiliser le code suivant pour définir le fichier :

```Javascript
let fs = require("fs"); //Importer le module "fs" pour la manipulation des fichiers
let file = "test.txt"; //Définir le nom du fichier à lire
```

Ensuite, il est important de spécifier l'encodage approprié pour le fichier. Dans cet exemple, nous utiliserons l'encodage "utf-8".

```Javascript
let encoding = "utf-8"; //Définir l'encodage du fichier
```

Maintenant que nous avons défini le fichier et l'encodage, nous pouvons lire le fichier en utilisant la méthode "readFile" du module "fs".

```Javascript
fs.readFile(file, encoding, function(err, data) {
    if (err) {
        return console.error(err);
    }
    console.log(data); //Afficher le contenu du fichier
    console.log("Lecture terminée");
});
```

Cette méthode utilise une fonction de rappel qui sera exécutée une fois que le fichier sera entièrement lu. Si une erreur se produit lors de la lecture, elle sera affichée dans la console. Sinon, le contenu du fichier sera affiché et un message indiquant que la lecture est terminée sera également affiché.

## Plongée en profondeur

Maintenant que vous savez comment lire un fichier texte en utilisant Javascript, il est important de comprendre certaines nuances. Par exemple, la méthode "readFile" est asynchrone, ce qui signifie que l'exécution du code se poursuivra sans attendre la fin de la lecture du fichier. Si vous avez besoin de lire un fichier de manière synchrone, c'est-à-dire en attendant que la lecture soit terminée avant de poursuivre l'exécution du code, vous pouvez utiliser la méthode "readFileSync" du module "fs".

De plus, la méthode "readFile" renvoie une chaîne de caractères, ce qui est utile si vous voulez simplement afficher le contenu du fichier ou effectuer des manipulations de base sur les données. Cependant, si vous avez besoin de traiter des données plus complexes, comme du JSON par exemple, vous devrez utiliser la méthode "readFile" en y spécifiant l'encodage approprié, puis utiliser la méthode "JSON.parse" pour convertir les données en objets JavaScript.

## Voir aussi

Pour plus d'informations sur la lecture de fichiers en Javascript, vous pouvez consulter les liens suivants :

- [Documentation officielle de Node.js sur le module fs](https://nodejs.org/api/fs.html)
- [Tutorial sur la lecture de fichiers en Javascript](https://www.digitalocean.com/community/tutorials/how-to-read-files-with-nodejs)
- [Exemples de lecture et écriture de fichiers en Javascript](https://www.tutorialsteacher.com/nodejs/nodejs-file-system)
---
title:                "TypeScript: Travailler avec les fichiers CSV"
simple_title:         "Travailler avec les fichiers CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec les fichiers CSV en TypeScript?

CSV, ou Comma-Separated Values, est un format de fichier très couramment utilisé pour stocker et échanger des données tabulaires. En utilisant TypeScript pour traiter ces fichiers, vous pouvez facilement manipuler et analyser de grandes quantités de données de manière organisée. C'est également un moyen très pratique pour importer et exporter des données à partir de différentes sources.

## Comment travailler avec les fichiers CSV en TypeScript?

Tout d'abord, vous devez installer le module `csv-parser` grâce à la commande `npm install csv-parser`. Ensuite, vous pouvez importer le module dans votre fichier TypeScript avec `import csvParser from 'csv-parser'`. Pour lire un fichier CSV, vous pouvez utiliser le code suivant :

```TypeScript
fs.createReadStream('chemin/vers/mon/fichier.csv')
  .pipe(csvParser())
  .on('data', (row) => {
    console.log(row);
  });
```

Ce code utilise la méthode `pipe()` pour lire le fichier et le module `csv-parser` pour le parseur en tant que flux de données. L'événement `data` est déclenché chaque fois qu'une ligne du fichier est lue, et le contenu de la ligne est affiché à l'aide de `console.log()`. Vous pouvez ensuite manipuler les données comme bon vous semble.

Pour écrire des données dans un fichier CSV, vous pouvez utiliser le module `csv-writer` en l'installant avec `npm install csv-writer`. Vous pouvez ensuite utiliser ce code comme exemple pour créer et écrire dans un fichier CSV :

```TypeScript
const createCsvWriter = require('csv-writer').createObjectCsvWriter;

const csvWriter = createCsvWriter({
  path: 'chemin/vers/mon/fichier.csv',
  header: [
    {id: 'nom', title: 'Nom de famille'},
    {id: 'prenom', title: 'Prénom'},
    {id: 'age', title: 'Âge'}
  ]
});

const données = [
  {nom: 'Dubois', prenom: 'Jean', age: 35},
  {nom: 'Martin', prenom: 'Marie', age: 27},
  {nom: 'Dupont', prenom: 'Pierre', age: 42}
];

csvWriter
  .writeRecords(données)
  .then(()=> console.log('Le fichier CSV a bien été écrit'));
```

Ce code crée un fichier CSV avec un en-tête et une liste de données. Vous pouvez personnaliser l'en-tête et les données selon vos besoins.

## Plongée profonde: Travailler avec les fichiers CSV en TypeScript

Travailler avec des fichiers CSV en TypeScript offre de nombreuses possibilités pour le traitement de données. Vous pouvez utiliser des fonctions telles que `fs.unlink()` pour supprimer des fichiers CSV, ou encore utiliser d'autres modules comme `csvtojson` pour convertir facilement un fichier CSV en un objet JSON.

Une autre fonctionnalité intéressante est le module `fast-csv`, qui permet de manipuler des données beaucoup plus rapidement que les autres modules. Il peut également être utilisé pour créer des fichiers CSV à partir de données en ligne.

Enfin, il est important de noter qu'il existe plusieurs packages disponibles pour travailler avec des fichiers CSV en TypeScript, chacun offrant ses propres fonctionnalités et avantages. Vous pouvez donc choisir celui qui correspond le mieux à vos besoins spécifiques.

## Voir aussi

- [Documentation de csv-parser](https://www.npmjs.com/package/csv-parser)
- [Documentation de csv-writer](https://www.npmjs.com/package/csv-writer)
- [Documentation de fast-csv](https://www.npmjs.com/package/fast-csv)
- [Documentation de csvtojson](https://www.npmjs.com/package/csvtojson)
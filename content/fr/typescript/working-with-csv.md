---
title:                "Travailler avec les fichiers csv"
html_title:           "TypeScript: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez régulièrement avec des données tabulaires, telles que des feuilles de calcul ou des bases de données, vous avez probablement rencontré le format CSV (Comma Separated Values). Il s'agit d'un format de fichier couramment utilisé pour stocker et échanger des données tabulaires. Bien que le CSV soit un format simple et couramment utilisé, il peut être fastidieux à manipuler manuellement, surtout lorsque vous travaillez avec de grandes quantités de données. C'est là que TypeScript peut vous être utile.

## Comment faire

En utilisant TypeScript, vous pouvez facilement lire, écrire et modifier des fichiers CSV de manière programmatique. Jetons un coup d'œil à un exemple concret en utilisant la bibliothèque `csv-parser` disponible sur NPM :

```TypeScript
// Importer la bibliothèque
import * as csv from 'csv-parser';
// Lire un fichier CSV et imprimer chaque ligne
fs.createReadStream('fichier.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('Lecture du fichier CSV terminée');
  });
```

Supposons que nous ayons un fichier CSV contenant les données suivantes :

```
Nom,Prenom,Age
Dupont,Alice,25
Durand,Paul,30
Martin,Lucie,28
```

En utilisant le code ci-dessus, nous pouvons obtenir la sortie suivante :

```
{ Nom: 'Dupont', Prenom: 'Alice', Age: '25' }
{ Nom: 'Durand', Prenom: 'Paul', Age: '30' }
{ Nom: 'Martin', Prenom: 'Lucie', Age: '28' }
```

Comme vous pouvez le constater, la bibliothèque `csv-parser` nous permet de lire facilement chaque ligne du fichier CSV et de la manipuler comme un objet JavaScript.

## Plongée en profondeur

Maintenant que nous avons vu comment lire un fichier CSV en utilisant TypeScript, parlons un peu de son fonctionnement. TypeScript est un langage de programmation open-source développé par Microsoft, basé sur JavaScript. Il fournit des fonctionnalités supplémentaires telles que le typage statique et la programmation orientée objet qui en font un langage intéressant pour manipuler des données structurées telles que le CSV.

Lorsque nous utilisons des bibliothèques telles que `csv-parser`, TypeScript nous permet de définir le type de données que nous voulons obtenir en résultat, ce qui rend notre code plus robuste et moins sujet aux erreurs. De plus, TypeScript est compilé en JavaScript, ce qui signifie que notre code peut s'exécuter sur n'importe quel navigateur ou environnement de serveur.

## Voir aussi

- [Site officiel de TypeScript](https://www.typescriptlang.org/)
- [`csv-parser` sur NPM](https://www.npmjs.com/package/csv-parser)
- [Tutoriel sur la manipulation de fichiers CSV avec TypeScript](https://www.codementor.io/@olatundegaruba/nodejs-csv-parser-tutorial-example-s3-du107z6zh#nodejs-file-and-csv)

Merci d'avoir lu ! N'hésitez pas à explorer davantage TypeScript pour découvrir toutes les possibilités qu'il offre pour travailler avec des données CSV.
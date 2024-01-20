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

# TypeScript: Travailler avec les fichiers CSV

## What & Why?

Travailler avec des fichiers CSV signifie manipuler des données de format Comma-Separated Values (valeurs séparées par des virgules). Les programmeurs utilisent souvent des fichiers CSV pour stocker et échanger des données tabulaires en utilisant un format simple et facilement lisible par l'ordinateur.

## How to:

Le code TypeScript suivant illustre comment lire et écrire des données à partir d'un fichier CSV en utilisant la bibliothèque "csv-parser". Assurez-vous d'installer cette bibliothèque en utilisant la commande `npm install csv-parser`.

```TypeScript
import fs from 'fs';
import csv from 'csv-parser';

//Lecture d'un fichier CSV
fs.createReadStream('mon_fichier.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('Lecture terminée');
  });

//Ecriture dans un fichier CSV
const data = [
  {
    nom: 'Jean',
    age: 32,
  },
  {
    nom: 'Marie',
    age: 28,
  },
];

fs.writeFileSync('nouveau_fichier.csv', csv(data));
```

La fonction `csv()` convertit automatiquement les objets JSON en un format CSV, tandis que `csv()` analyse les données CSV et les transforme en objets JSON.

## Deep Dive

Les fichiers CSV ont été créés dans les années 1970 pour faciliter l'échange de données entre les programmes informatiques. Aujourd'hui, les fichiers CSV restent très populaires en raison de leur simplicité et de leur compatibilité avec de nombreuses applications.

Il existe également d'autres types de fichiers tabulaires, tels que les fichiers Excel, mais ceux-ci peuvent être plus difficiles à manipuler en tant que développeur. Les fichiers CSV sont également largement utilisés pour l'importation et l'exportation de données dans des bases de données.

La bibliothèque "csv-parser" utilisée dans l'exemple ci-dessus est basée sur un lecteur de flux, ce qui signifie que les données CSV sont lues de manière asynchrone et en continu, ce qui peut être plus efficace pour les gros fichiers.

## See Also

- [Documentation de la bibliothèque "csv-parser"](https://csv.js.org/parse/)
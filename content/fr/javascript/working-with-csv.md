---
title:                "Javascript: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Le format CSV (Comma Separated Values) est un outil précieux pour les programmeurs, car il permet de stocker et de manipuler des données facilement dans un format simple et lisible par les humains.

## Comment faire

Pour lire un fichier CSV dans un programme Javascript, il faut d'abord utiliser la fonction `require` pour charger le module `fs`, qui nous permet d'accéder aux fichiers système. Ensuite, il faut utiliser la fonction `readFile` pour lire le fichier CSV et le stocker dans une variable.

````Javascript
const fs = require('fs');
const csv = fs.readFile('fichier.csv', 'utf8');
````

Ensuite, nous pouvons utiliser la méthode `split` pour séparer les données en lignes et en colonnes, en utilisant le caractère `,` comme séparateur. Nous pouvons aussi utiliser la méthode `map` pour parcourir toutes les données et créer un tableau avec les valeurs de chaque colonne.

````Javascript
const lignes = csv.split('\n');
const donnees = lignes.map(ligne => ligne.split(','));
````

Enfin, nous pouvons afficher les données dans la console en utilisant la méthode `console.table` et en passant en paramètre notre tableau de données.

````Javascript
console.table(donnees);
````

Voici un exemple de données dans un fichier CSV et le résultat obtenu dans la console :

````Javascript
// donnees.csv
nom,prenom,age
Dupont,Alain,30
Martin,Sophie,25
Lacroix,Jean,35

// Console output
┌─────────┬─────────┬────┐
│ (index) │ nom     │ age │
├─────────┼─────────┼────┤
│ 0       │ 'Dupont'│ 30  │
│ 1       │ 'Martin'│ 25  │
│ 2       │ 'Lacroix' │ 35 │
└─────────┴─────────┴────┘
````

## Plongée en profondeur

Il existe de nombreuses librairies Javascript dédiées à la manipulation de fichiers CSV, telles que `csv-parser` ou `csvtojson`. Ces librairies offrent des fonctionnalités avancées pour gérer des fichiers de grande taille, des données en streaming, ou pour effectuer des opérations telles que le tri et la fusion de fichiers CSV.

De plus, il peut être utile de connaître les spécifications du format CSV, en particulier en ce qui concerne les différents caractères de séparation et les règles pour les données contenant des virgules ou des guillemets.

## Voir aussi

- [Documentation officielle de Node.js sur la manipulation de fichiers CSV](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [Librairie csv-parser sur GitHub](https://github.com/mafintosh/csv-parser)
- [Spécifications du format CSV](https://tools.ietf.org/html/rfc4180)
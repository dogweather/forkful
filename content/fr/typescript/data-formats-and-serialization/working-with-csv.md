---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:24.853526-07:00
description: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ implique la lecture et l'\xE9criture de fichiers CSV, un format d'\xE9change de\
  \ donn\xE9es\u2026"
lastmod: '2024-02-25T18:49:54.275832-07:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ implique la lecture et l'\xE9criture de fichiers CSV, un format d'\xE9change de\
  \ donn\xE9es\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Travailler avec des fichiers CSV (Valeurs Séparées par des Virgules) implique la lecture et l'écriture de fichiers CSV, un format d'échange de données courant utilisé en raison de sa simplicité et de son large support à travers diverses plateformes et langages. Les programmeurs traitent des fichiers CSV pour importer ou exporter des données depuis des applications, des bases de données et des services, facilitant ainsi la manipulation et le partage de données.

## Comment faire :

En TypeScript, vous pouvez travailler avec des fichiers CSV soit par du code natif, soit en utilisant des bibliothèques tierces comme `csv-parser` pour la lecture et `csv-writer` pour l'écriture de fichiers CSV.

### Lire un fichier CSV avec `csv-parser`

D'abord, installez `csv-parser` via npm :

```
npm install csv-parser
```

Ensuite, lisez un fichier CSV comme suit :

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Sortie : Tableau d'objets, chacun représentant une ligne dans le fichier CSV
  });
```

En supposant que `data.csv` contient :

```
name,age
Alice,30
Bob,25
```

La sortie sera :

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### Écrire dans un fichier CSV avec `csv-writer`

Pour écrire dans un fichier CSV, installez d'abord `csv-writer` :

```
npm install csv-writer
```

Puis, utilisez-le comme suit :

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NOM'},
    {id: 'age', title: 'ÂGE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('Le fichier CSV a été écrit avec succès'));
```

Ce code écrit ce qui suit dans `out.csv` :

```
NOM,ÂGE
Alice,30
Bob,25
```

Ces exemples montrent comment intégrer le traitement de fichiers CSV dans vos projets TypeScript de manière efficace, que ce soit pour lire des données en vue d'une analyse ou pour persister des données d'application de manière externe.

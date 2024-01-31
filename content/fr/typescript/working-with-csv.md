---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Le CSV, ou "Comma-Separated Values", est un format de fichier pour stocker des données tabulaires. Les programmeurs l'utilisent pour la simplicité d'échange de données entre des systèmes et des applications.

## How to:
Pour gérer les fichiers CSV en TypeScript, on peut utiliser la bibliothèque `papaparse`. Voici comment lire et écrire des CSV.

```TypeScript
import Papa from 'papaparse';

// Lire un CSV
const csvFile = `prenom,nom,age
Thomas,Durand,28
Julie,Moreau,33`;

Papa.parse(csvFile, {
  header: true,
  complete: (results) => {
    console.log('Résultats:', results.data);
  }
});

// Écrire un CSV
const data = [
  { prenom: 'Marc', nom: 'Lavoine', age: 22 },
  { prenom: 'Sophie', nom: 'Ferrand', age: 45 }
];

const csv = Papa.unparse(data);
console.log('CSV Généré:', csv);
```

Output pour la lecture:
```
Résultats: [
  { prenom: 'Thomas', nom: 'Durand', age: '28' },
  { prenom: 'Julie', nom: 'Moreau', age: '33' }
]
```

Output pour l'écriture:
```
CSV Généré: prenom,nom,age
Marc,Lavoine,22
Sophie,Ferrand,45
```

## Deep Dive
Le format CSV existe depuis les premières années de l'informatique personnelle. Des alternatives incluent JSON ou XML, mais le CSV reste populaire pour sa lisibilité et sa facilité d'import/export dans des tableurs. La plupart des langages de programmation ont des bibliothèques pour gérer les CSV, en TypeScript, `papaparse` offre un bon équilibre entre simplicité et fonctionnalités.

## See Also
- La documentation de 'papaparse': https://www.papaparse.com/docs
- Spécifications RFC 4180 pour CSV: https://tools.ietf.org/html/rfc4180
- Manipulation de CSV avec Node.js: https://nodejs.org/api/fs.html

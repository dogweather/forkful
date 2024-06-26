---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:53.913724-07:00
description: "Comment faire : Pour convertir une cha\xEEne JSON en un objet JavaScript,\
  \ utilisez `JSON.parse()`."
lastmod: '2024-03-13T22:44:58.300392-06:00'
model: gpt-4-0125-preview
summary: "Pour convertir une cha\xEEne JSON en un objet JavaScript, utilisez `JSON.parse()`."
title: Travailler avec JSON
weight: 38
---

## Comment faire :


### Analyser du JSON
Pour convertir une chaîne JSON en un objet JavaScript, utilisez `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Sortie: John
```

### Transformer des Objets JavaScript en Chaînes JSON
Pour reconvertir un objet JavaScript en une chaîne JSON, utilisez `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Sortie: {"name":"Jane","age":25,"city":"London"}
```

### Travailler avec des Fichiers dans Node.js
Pour lire un fichier JSON et le convertir en un objet dans un environnement Node.js, vous pouvez utiliser le module `fs`. Cet exemple suppose que vous avez un fichier nommé `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

Pour écrire un objet dans un fichier JSON :

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('Données écrites dans le fichier');
});
```

### Bibliothèques Tiers
Pour des opérations JSON complexes, des cadres et bibliothèques comme `lodash` peuvent simplifier les tâches, mais pour des opérations de base, les fonctions JavaScript natives sont souvent suffisantes. Pour des applications à grande échelle ou critiques en termes de performance, vous pouvez considérer des bibliothèques comme `fast-json-stringify` pour une sérialisation JSON plus rapide ou `json5` pour l'analyse et la sérialisation en utilisant un format JSON plus flexible.

Analyse avec `json5` :
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Sortie: John
```

Ces exemples couvrent les opérations de base avec JSON en JavaScript, parfait pour les débutants en transition d'autres langues et cherchant à gérer les données dans les applications Web efficacement.

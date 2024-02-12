---
title:                "Travailler avec YAML"
aliases:
- fr/javascript/working-with-yaml.md
date:                  2024-02-03T19:26:01.609191-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

YAML, abréviation de YAML Ain't Markup Language, est un format de sérialisation de données lisible par l'homme. Les programmeurs l'utilisent souvent pour les fichiers de configuration et l'échange de données entre langages en raison de sa simplicité et de sa lisibilité par rapport à JSON ou XML.

## Comment faire :

En JavaScript, travailler avec YAML implique généralement l'utilisation d'une bibliothèque tierce puisque le langage n'inclut pas d'analyseur intégré pour YAML. L'une des bibliothèques les plus populaires à cet effet est `js-yaml`. Vous pouvez utiliser `js-yaml` pour analyser le YAML en objets JavaScript et vice versa.

Tout d'abord, vous devez installer `js-yaml` :

```bash
npm install js-yaml
```

Ensuite, vous pouvez l'utiliser dans vos projets. Voici comment vous pouvez charger un fichier YAML et l'analyser en un objet JavaScript :

```javascript
// Requérir le module js-yaml
const yaml = require('js-yaml');
const fs   = require('fs');

// Charger le YAML à partir d'un fichier
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Si votre fichier `config.yaml` ressemble à cela :

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

La sortie sera :

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

Pour faire l'inverse, convertir un objet JavaScript en une chaîne YAML :

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Ce code produira :

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

En utilisant `js-yaml`, vous pouvez facilement intégrer l'analyse et la sérialisation YAML dans vos projets JavaScript, améliorant l'interéchangeabilité des données et la gestion de la configuration.

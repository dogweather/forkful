---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:01.609191-07:00
description: "YAML, abr\xE9viation de YAML Ain't Markup Language, est un format de\
  \ s\xE9rialisation de donn\xE9es lisible par l'homme. Les programmeurs l'utilisent\
  \ souvent pour\u2026"
lastmod: '2024-03-13T22:44:58.299211-06:00'
model: gpt-4-0125-preview
summary: "YAML, abr\xE9viation de YAML Ain't Markup Language, est un format de s\xE9\
  rialisation de donn\xE9es lisible par l'homme. Les programmeurs l'utilisent souvent\
  \ pour\u2026"
title: Travailler avec YAML
weight: 41
---

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

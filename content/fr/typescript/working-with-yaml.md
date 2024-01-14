---
title:                "TypeScript: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Le YAML est un format de données populaire dans le développement web moderne en raison de sa facilité de lecture et de sa flexibilité. En utilisant TypeScript pour travailler avec des fichiers YAML, vous pouvez améliorer votre flux de travail de développement et créer des applications plus efficacement.

## Comment faire

Pour travailler avec du YAML en TypeScript, il existe une bibliothèque appelée "js-yaml" qui peut être installée à l'aide de npm. Après avoir importé la bibliothèque, vous pouvez utiliser sa méthode "safeLoad" pour charger un fichier YAML en tant qu'objet JavaScript. Par exemple :

```TypeScript
import * as yaml from 'js-yaml';
const file = yaml.safeLoad('exemple.yaml');
console.log(file);
// output : { test: 'valeur' }
```

Pour modifier un fichier YAML en TypeScript, vous pouvez d'abord le charger en tant qu'objet JavaScript, le modifier selon vos besoins, puis le convertir à nouveau en YAML en utilisant la méthode "safeDump". Par exemple :

```TypeScript
import * as yaml from 'js-yaml';
const file = yaml.safeLoad('exemple.yaml');
file.test = 'nouvelle valeur';
console.log(yaml.safeDump(file));
// output : test: nouvelle valeur
```

## Approfondissement

En plus des méthodes de base de la bibliothèque "js-yaml", il existe également des options pour gérer des objets plus complexes et pour valider le format du fichier YAML. Vous pouvez consulter la documentation officielle pour plus d'informations et pour découvrir toutes les fonctionnalités offertes par cette bibliothèque.

## Voir aussi

- [Documentation officielle de la bibliothèque js-yaml](https://github.com/nodeca/js-yaml)
- [Exemples de traitement de fichiers YAML avec TypeScript](https://github.com/tomclarkson94/yaml-typescript-examples)
- [Tutoriel complet sur l'utilisation de YAML avec TypeScript](https://dev.to/mikebild/working-with-yaml-and-node-js-da9)
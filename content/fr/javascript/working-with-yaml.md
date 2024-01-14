---
title:                "Javascript: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec Javascript, il est probable que vous ayez déjà entendu parler de YAML. Mais pourquoi devriez-vous vous intéresser à ce format de données ? Et bien, YAML est un langage de sérialisation de données qui est très utile lorsque vous avez besoin de stocker et de transporter des données complexes dans vos projets Javascript. Il est également facile à lire pour les humains, ce qui facilite la compréhension et la gestion des données.

Maintenant que vous savez pourquoi YAML est utile, passons à la façon de l'utiliser dans vos projets.

## Comment faire

Pour commencer à travailler avec YAML dans votre code Javascript, vous devrez d'abord télécharger le package nodejs-yaml à l'aide de la commande NPM suivante :

```Javascript
npm install js-yaml --save
```

Une fois le package installé, vous pourrez l'importer dans votre code Javascript en utilisant la commande suivante :

```Javascript
const yaml = require('js-yaml');
```

Maintenant, vous pouvez utiliser différentes méthodes pour convertir des données dans des formats tels que JSON et YAML, ou vice versa. Par exemple, pour convertir une chaîne YAML en objet javascript, vous pouvez utiliser la méthode `safeLoad()` comme ceci :

```Javascript
var yamlString = `
  name: John
  age: 30
  hobbies:
    - reading
    - hiking
    - cooking`;

var jsObject = yaml.safeLoad(yamlString);

console.log(jsObject); // { name: 'John', age: 30, hobbies: ['reading', 'hiking', 'cooking'] }
```

Il est également possible de convertir un objet javascript en chaîne YAML avec la méthode `safeDump()` :

```Javascript
var jsObject = { name: 'Jane', age: 25, hobbies: ['yoga', 'painting'] };

var yamlString = yaml.safeDump(jsObject);

console.log(yamlString); // name: Jane, age: 25, hobbies: [ yoga, painting ]
```

Vous pouvez également utiliser ces méthodes pour convertir des fichiers JSON en fichiers YAML, ou vice versa.

## Plongée en profondeur

Si vous souhaitez en savoir plus sur YAML et ses différents usages, vous pouvez consulter les documentations officielles du package [nodejs-yaml](https://www.npmjs.com/package/js-yaml) et de la spécification [YAML](https://yaml.org/spec/). Vous pouvez également expérimenter et pratiquer en utilisant différentes données et formats pour mieux comprendre son fonctionnement.

## Voir aussi

- [La documentation officielle du package nodejs-yaml](https://www.npmjs.com/package/js-yaml)
- [La spécification officielle de YAML](https://yaml.org/spec/)
---
title:                "Travailler avec yaml"
html_title:           "TypeScript: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur travaillant avec TypeScript, vous avez probablement déjà entendu parler de YAML. YAML est un langage de sérialisation de données léger et facile à lire, ce qui le rend très utile pour configurer des applications et des environnements de développement. Dans cet article, nous allons explorer comment utiliser TypeScript pour travailler avec YAML et comment cela peut améliorer votre processus de développement.

## Comment faire

Pour utiliser YAML avec TypeScript, vous devez tout d'abord installer un package appelé `js-yaml`. Vous pouvez le faire en utilisant la commande suivante dans votre terminal :

```
npm install js-yaml
```

Ensuite, dans votre code TypeScript, vous devez importer le package `js-yaml` et l'utiliser pour charger votre fichier YAML comme suit :

```TypeScript
import * as yaml from 'js-yaml';
import * as fs from 'fs';

const config = yaml.safeLoad(fs.readFileSync('config.yml', 'utf8'));
```

Ce code va charger le fichier `config.yml` et le stocker dans la variable `config` sous forme d'objet JavaScript. Vous pouvez maintenant utiliser cet objet pour accéder aux valeurs de votre fichier YAML, telles que des clés et des valeurs.

Si vous souhaitez écrire un objet JavaScript sous la forme d'un fichier YAML, vous pouvez utiliser la fonction `safeDump` de `js-yaml`. Voici un exemple de code :

```TypeScript
import * as yaml from 'js-yaml';
import * as fs from 'fs';

const data = {
  name: 'John',
  age: 25,
  hobbies: ['programming', 'hiking'],
};

const ymlString = yaml.safeDump(data);
fs.writeFileSync('output.yml', ymlString, 'utf8');
```

Le fichier `output.yml` contiendra maintenant les données de l'objet JavaScript sous forme de YAML.

## Plongée en profondeur

En travaillant avec YAML en utilisant TypeScript, il est important de comprendre comment les types sont gérés. Comme YAML est principalement basé sur les données et ne possède pas de type spécifique, TypeScript peut parfois avoir des difficultés à inférer correctement les types pour les valeurs des fichiers YAML.

Pour résoudre ce problème, vous pouvez utiliser des annotations de type dans votre code TypeScript pour spécifier les types que vous attendez pour chaque clé dans votre fichier YAML. Par exemple, si vous avez une clé `age` qui doit être un nombre, vous pouvez l'annoter comme ceci :

```TypeScript
type Config = { name: string; age: number; hobbies: string[] };

const config: Config = yaml.safeLoad(fs.readFileSync('config.yml', 'utf8'));
```

En utilisant ce type de déclaration, TypeScript sera en mesure de vérifier si les types dans votre fichier YAML correspondent bien aux types spécifiés.

## Voir aussi

- Site officiel de YAML : https://yaml.org/
- Documentation de TypeScript : https://www.typescriptlang.org/docs/
- Documentation de `js-yaml` : https://www.npmjs.com/package/js-yaml
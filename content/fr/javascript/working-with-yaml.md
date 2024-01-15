---
title:                "Travailler avec yaml"
html_title:           "Javascript: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être familier avec le format de données JSON en JavaScript, mais saviez-vous qu'il existe également un format de données appelé YAML? Bien que moins courant, YAML offre de nombreux avantages pour le stockage et la lecture de configurations complexes. Dans cet article, nous allons vous montrer comment travailler avec YAML en JavaScript et pourquoi vous devriez envisager de l'utiliser dans vos projets.

## Comment faire

Pour commencer à travailler avec YAML en JavaScript, vous devrez installer un module appelé YAML. Vous pouvez le faire en utilisant le gestionnaire de packages NPM en tapant la commande suivante dans votre terminal :

```
npm install yaml
```

Une fois le module installé, vous pouvez l'importer dans votre code JavaScript en utilisant la syntaxe suivante :

```
const yaml = require('yaml');
```

Maintenant, vous pouvez utiliser la méthode `parse` pour convertir une chaîne de caractères YAML en un objet JavaScript. Voici un exemple :

```
const yamlStr = `name: John
age: 25`;

const obj = yaml.parse(yamlStr);
console.log(obj.name); // Output: John
```

Vous pouvez également utiliser la méthode `stringify` pour convertir un objet JavaScript en une chaîne de caractères YAML. Voici un exemple :

```
const obj = { name: 'John', age: 25 };

const yamlStr = yaml.stringify(obj);
console.log(yamlStr); // Output: "name: John\age: 25"
```

Ces méthodes sont très utiles pour lire et écrire des fichiers de configuration YAML dans vos projets JavaScript. Vous pouvez également utiliser la méthode `load` pour charger un fichier YAML directement en mémoire, ce qui peut être pratique pour les fichiers de grande taille.

## Plongée en profondeur

En plus des fonctionnalités de base pour lire et écrire des fichiers YAML, le module YAML offre également des fonctionnalités avancées telles que la validation des données et la personnalisation de la syntaxe YAML. Vous pouvez utiliser la méthode `schema` pour créer un schéma de validation et valider vos données YAML en utilisant ce schéma. Vous pouvez également utiliser la méthode `setSchema` pour personnaliser la syntaxe YAML en définissant vos propres balises et en créant des documents YAML avec cette syntaxe personnalisée.

Il est également important de noter que YAML prend en charge les commentaires, ce qui peut être très utile pour fournir des explications ou des notes pour votre configuration. Vous pouvez utiliser la notation `#` pour ajouter un commentaire à la fin d'une ligne ou utiliser la notation `|` pour ajouter un commentaire sur plusieurs lignes. Les commentaires seront ignorés lors de la lecture du fichier YAML, mais ils resteront visibles pour une meilleure compréhension du fichier.

## Voir aussi

- [Documentation du module YAML](https://github.com/eemeli/yaml)
- [Tutoriel sur la manipulation des fichiers YAML en JavaScript](https://www.digitalocean.com/community/tutorials/how-to-use-yaml-to-manipulate-a-configuration-file-in-javascript)
- [Vidéo sur l'utilisation de YAML dans un projet JavaScript réel](https://www.youtube.com/watch?v=a4ZSwI8bPSU)

Merci d'avoir lu cet article sur l'utilisation de YAML en JavaScript. Nous espérons que vous pourrez maintenant profiter de ses avantages dans vos projets futurs. Happy coding!
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

## Qu'est-ce que c'est et pourquoi on le fait?
Travailler avec YAML est une façon de gérer des données de configuration ou de structure de manière facilement lisible. Les programmeurs l'utilisent parce qu'il est plus simple à lire et à écrire que le JSON ou le XML.

## Comment faire:
Voici un exemple de code TypeScript utilisant la librairie YAML pour parser un fichier YAML et afficher le contenu dans la console:

```TypeScript
import * as yaml from "yaml";

const data = `
  nom: Jean
  age: 25
  ville: Paris
`;

const parsedData = yaml.parse(data); // convertit le fichier YAML en objet JavaScript
console.log(parsedData); // affiche {nom: "Jean", age: 25, ville: "Paris"}
```

## Plongée en profondeur:
YAML a été créé en 2001 dans le but de fournir un moyen simple et lisible de stocker des données structurées. D'autres alternatives telles que JSON et XML sont également couramment utilisées, mais YAML se distingue par sa facilité de lecture pour les humains. En termes d'implémentation, YAML utilise une syntaxe indentée pour définir la structure des données.

## Voir aussi:
- [Documentation officielle de YAML](https://yaml.org/)
- [Comparaison entre YAML, JSON et XML](https://www.baeldung.com/java-yaml-json-xml)
- [Guide de démarrage de TypeScript](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
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

# Qu'est-ce que c'est et pourquoi le JavaScript utilise-t-il le YAML? 
Le YAML (YAML Ain't Markup Language) est un format de données structuré utilisé pour stocker et échanger des informations. Il est souvent utilisé par les programmeurs dans le développement web pour créer des fichiers de configuration et les échanger entre différentes applications. Il est facile à lire et à écrire, ce qui en fait un choix populaire pour les développeurs.

## Comment faire: 
Voici un exemple simple de code utilisant le YAML en JavaScript:

```javascript
// Déclaration d'un objet YAML
let user = {
  name: 'Jean',
  age: 26,
  interests: ['programmation', 'jeux vidéo']
};

// Conversion de l'objet en chaîne YAML
let yamlString = YAML.stringify(user);

// Résultat: name: Jean | age: 26 | interests: - programmation | - jeux vidéo
```

## Plongée en profondeur: 
Le YAML a été créé en 2001 par Clark Evans et Ingy döt Net pour simplifier la création de fichiers de configuration. Il est basé sur le langage de programmation PERL et se présente sous la forme d'un document texte lisible par l'homme. Il existe également des alternatives telles que JSON (JavaScript Object Notation) et XML (Extensible Markup Language). En JavaScript, vous pouvez utiliser des bibliothèques telles que YAML.js pour travailler avec du code YAML.

## Voir aussi: 
Pour en savoir plus sur le YAML, vous pouvez consulter le site officiel à l'adresse suivante: https://yaml.org/. Vous pouvez également trouver des tutoriels et des exemples de code en utilisant YAML en JavaScript sur des sites comme Codecademy (https://www.codecademy.com/learn/learn-yaml) et W3schools (https://www.w3schools.com/js/js_yaml.asp).
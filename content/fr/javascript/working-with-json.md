---
title:                "Travailler avec json"
html_title:           "Javascript: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 
Travailler avec JSON (JavaScript Object Notation) consiste à manipuler des données sous forme de textes structurés. Les programmeurs utilisent couramment JSON car il s'agit d'un format léger, facile à lire et à écrire, et compatible avec de nombreux langages de programmation. 

## Comment faire:
Pour créer un objet JSON en JavaScript, utilisez la méthode `JSON.stringify()` en lui passant un objet en argument. Par exemple: 
```Javascript
const voiture = { marque: 'Mercedes', modèle: 'C-Class', année: 2020 };
const voitureJSON = JSON.stringify(voiture);
console.log(voitureJSON);
```
Cela produira la sortie suivante: 
```
{"marque": "Mercedes", "modèle": "C-Class", "année": 2020}
```

Pour convertir un objet JSON en objet JavaScript, utilisez la méthode `JSON.parse()` en lui passant le texte JSON en argument. Par exemple:
```Javascript
const voitureJSON = '{"marque": "Mercedes", "modèle": "C-Class", "année": 2020}';
const voiture = JSON.parse(voitureJSON);
console.log(voiture.marque);
```
Cela produira la sortie suivante: 
```
Mercedes
```

## Plongeons plus en détail: 
JSON a été développé en 2001 par Douglas Crockford et est devenu l'un des formats les plus populaires pour échanger des données entre différentes applications. Avant JSON, le format XML était couramment utilisé pour cela, mais il s'est avéré être trop verbeux et difficile à lire pour les humains. En plus de JavaScript, JSON est également supporté par de nombreux autres langages tels que Python, PHP et Java.

Il existe également des alternatives à JSON telles que YAML et TOML, mais JSON reste le choix privilégié en raison de sa simplicité et de sa compatibilité. JSON est également utilisé pour le stockage de données et les API (Application Programming Interface) car il est facilement lisible et interprétable par les machines.

## À voir également: 
- [Documentation officielle JSON](https://json.org)
- [Introduction à JSON](https://www.w3schools.com/js/js_json_intro.asp)
- [Comparaison de JSON avec d'autres formats de données](https://www.slant.co/versus/1227/1319/~json_vs_yaml)
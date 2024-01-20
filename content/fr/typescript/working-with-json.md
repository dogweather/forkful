---
title:                "Travailler avec JSON"
html_title:           "TypeScript: Travailler avec JSON"
simple_title:         "Travailler avec JSON"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est ? 
Le JSON (JavaScript Object Notation) est un format de données populaire utilisé par les programmeurs pour échanger des informations structurées. Il est souvent utilisé pour stocker et transmettre des données entre différentes applications et systèmes. 
## Pourquoi le JSON est-il couramment utilisé ?
Le JSON est un format de données léger, facile à lire et à écrire pour les humains ainsi que pour les machines. Il est également pris en charge par de nombreux langages de programmation, y compris TypeScript, ce qui le rend largement accessible pour les programmeurs.
## Comment travailler avec JSON en TypeScript :
Voici un exemple simple de création d'un objet JSON en TypeScript :
```
let user = {
    name: "John",
    age: 30,
    city: "Paris"
};
```
Pour accéder aux propriétés de cet objet, on peut utiliser la notation du point ou des crochets :
```
user.name // John
user["age"] // 30
```
On peut également convertir un objet JSON en chaîne de caractères à l'aide de la méthode `JSON.stringify()` :
```
let userString = JSON.stringify(user);
// {"name":"John","age":30,"city":"Paris"}
```
Et pour convertir une chaîne de caractères en objet JSON, on utilise la méthode `JSON.parse()` :
```
let newUser = JSON.parse('{"name":"Jane","age":25,"city":"Lyon"}');
```
## Plongée en profondeur : 
Le JSON a été créé en 2001 par Douglas Crockford, et a rapidement gagné en popularité en raison de sa simplicité et de sa flexibilité. Il a rapidement remplacé d'autres formats de données tels que XML pour les applications Web et les services Web. Bien qu'il existe de nombreuses alternatives, le JSON reste un choix populaire en raison de sa compatibilité avec de nombreux langages de programmation et sa facilité d'utilisation.
Pour travailler avec des données JSON en TypeScript, on peut utiliser la bibliothèque `JSON` intégrée qui fournit des méthodes pour traiter les données JSON telles que `stringify()` et `parse()`. TypeScript prend également en charge l'utilisation des types définis par l'utilisateur pour représenter des données JSON afin de faciliter la lecture et la validation des données.
## Voir aussi : 
- [Le site officiel de JSON](https://www.json.org/)
- [Le cours interactif de Codecademy sur le JSON](https://www.codecademy.com/learn/learn-json)
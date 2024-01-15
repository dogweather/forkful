---
title:                "Travailler avec JSON"
html_title:           "Javascript: Travailler avec JSON"
simple_title:         "Travailler avec JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur web, il est fort probable que vous travailliez avec JSON sur une base quotidienne. En utilisant le JavaScript, vous pouvez facilement traiter et manipuler les données au format JSON, ce qui rend votre travail plus efficace et plus fluide.

## Comment Faire

Pour commencer, nous allons définir ce qu'est JSON. JSON, ou JavaScript Object Notation, est un format de données largement utilisé pour stocker et échanger des informations sur le web. Il est basé sur la syntaxe du JavaScript, ce qui le rend facile à utiliser avec ce langage de programmation.

Pour créer un objet JSON en JavaScript, nous pouvons utiliser la méthode `JSON.stringify()`. Par exemple:

```Javascript
let person = {
  name: 'John',
  age: 30,
  city: 'Paris'
};

let jsonPerson = JSON.stringify(person);
console.log(jsonPerson);
```

Dans cet exemple, nous avons créé un objet `person` avec des propriétés telles que `name`, `age` et `city`. En utilisant la méthode `JSON.stringify()`, nous avons converti cet objet en une chaîne de caractères JSON, qui est ensuite stockée dans la variable `jsonPerson`. Ensuite, nous avons affiché cette chaîne de caractères dans la console.

Pour accéder aux données d'un objet JSON, nous pouvons utiliser la méthode `JSON.parse()`. Par exemple:

```Javascript
let jsonCar = '{"brand":"Toyota", "model":"Camry", "year":2015}';
let car = JSON.parse(jsonCar);
console.log(car.brand); // output: Toyota
```

Ici, nous avons une chaîne de caractères JSON qui représente un objet de voiture. En utilisant la méthode `JSON.parse()`, nous avons converti la chaîne en un objet JavaScript et pu ensuite accéder à ses propriétés telles que `brand`, `model` et `year`.

## Plongée en Profondeur

Il est important de noter que JSON peut être utilisé pour représenter des données de manière hiérarchique en utilisant des objets et des tableaux imbriqués. Par exemple:

```Javascript
let person = {
  name: 'John',
  age: 30,
  address: {
    street: '123 Main St',
    city: 'Paris',
    country: 'France'
  },
  hobbies: ['reading', 'cooking', 'traveling']
};
```

Dans cet exemple, la propriété `address` est un objet imbriqué contenant plusieurs propriétés telles que `street`, `city` et `country`. De même, la propriété `hobbies` est un tableau contenant une liste de passe-temps.

De plus, JSON prend également en charge les valeurs nulles (`null`) ainsi que les types de données tels que les chaînes de caractères, les nombres, les booléens et les tableaux.

Il est également possible de parcourir et de modifier les données JSON en utilisant des boucles `for` ou des méthodes comme `forEach` ou `map`.

## Voir Aussi

- [Documentation officielle de JSON](https://www.json.org/json-en.html)
- [Didacticiels JSON sur W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Article sur les objets JSON en JavaScript sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/JSON)
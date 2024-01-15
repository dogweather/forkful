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

## Pourquoi

Si vous travaillez avec des données, il est très probable que vous allez rencontrer le format de données JSON. Il est important de savoir comment travailler avec ce format de manière efficace afin de manipuler vos données de manière appropriée.

## Comment faire

Pour commencer à travailler avec JSON en TypeScript, vous devrez tout d'abord définir un type pour vos données JSON. Par exemple, si vous avez un objet avec deux propriétés "nom" et "âge", votre type sera le suivant :

```TypeScript
type Person = {
  name: string,
  age: number
}
```

Ensuite, vous pouvez utiliser la fonction "JSON.parse()" pour convertir une chaîne de caractères JSON en un objet TypeScript utilisant votre type défini. Voici un exemple :

```TypeScript
let jsonString = '{"name":"John", "age":30}';
let person: Person = JSON.parse(jsonString);
console.log(person.name); // affiche "John"
console.log(person.age); // affiche 30
```

Si vous voulez convertir un objet TypeScript en chaîne de caractères JSON, vous pouvez utiliser la fonction "JSON.stringify()". Par exemple :

```TypeScript
let person: Person = {
  name: "Jane",
  age: 25
};
let jsonString = JSON.stringify(person);
console.log(jsonString); // affiche '{"name":"Jane", "age":25}'
```

## Plongée en profondeur

En travaillant avec JSON en TypeScript, il est important de comprendre comment le type "any" fonctionne. Si vous déclarez une variable en tant que type "any", elle peut avoir n'importe quel type de valeur, y compris une valeur JSON. Cela peut être utile si vous voulez manipuler ou accéder à une propriété spécifique d'un objet JSON sans définir un type spécifique pour cet objet.

De plus, si vous travaillez avec des tableaux JSON, vous pouvez utiliser l'opérateur "as" pour dire à TypeScript de traiter un tableau comme un type spécifique. Par exemple :

```TypeScript
let jsonArray: any = ['Bonjour', 'Hello', 'Hola'];
let greetings: string[] = jsonArray as string[];
console.log(greetings[0]); // affiche "Bonjour"
console.log(greetings[1]); // affiche "Hello"
```

## Voir aussi

- [Documentation officielle de TypeScript sur JSON](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html#json)
- [Introduction à TypeScript pour les débutants](https://dev.to/techcater/understanding-typescript-basics-for-beginners-part-1-3afb)
- [Tutoriel sur l'utilisation de JSON en TypeScript](https://ultimatecourses.com/blog/getting-started-with-type-script-working-with-json)
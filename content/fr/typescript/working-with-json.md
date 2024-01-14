---
title:                "TypeScript: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec JSON

JSON (JavaScript Object Notation) est un format de fichier léger et facile à lire qui est très utilisé dans le développement web. En travaillant avec JSON, vous pouvez facilement échanger des données entre différentes applications et plates-formes, ce qui le rend très utile pour les développeurs.

## Comment faire

Pour travailler avec JSON en TypeScript, il existe plusieurs façons de le faire. Vous pouvez créer un objet JSON à partir de zéro en utilisant la syntaxe suivante :

```TypeScript
let myJSON = { "name": "John", "age": 30, "city": "Paris" };
```

Vous pouvez également convertir un objet en JSON en utilisant la fonction `stringify` :

```TypeScript
let myObj = { "name": "John", "age": 30, "city": "Paris" };
let myJSON = JSON.stringify(myObj);
```

Pour accéder aux données dans un fichier JSON externe, vous pouvez utiliser la fonction `fetch` pour récupérer le fichier et ensuite utiliser `json()` pour le convertir en objet JSON :

```TypeScript
fetch('myJsonFile.json')
  .then(response => response.json())
  .then(data => console.log(data));
```

## Plongée profonde

Lorsque vous travaillez avec JSON en TypeScript, il est important de comprendre sa structure de base. Un objet JSON est constitué de paires de clés et de valeurs, où la valeur peut être un autre objet JSON ou une valeur primitive (string, number, boolean).

Il est également important de noter que la syntaxe JSON est différente de la syntaxe JavaScript. Par exemple, dans JSON, les noms de clés doivent être entre guillemets, tandis que dans JavaScript, ils peuvent être sans guillemets.

Un autre aspect important à connaître est que la fonction `stringify` peut accepter un troisième paramètre optionnel pour formater la sortie JSON de manière lisible pour les humains. Cela peut être utile pour le débogage.

## Voir aussi

- [Documentation officielle de TypeScript sur JSON](https://www.typescriptlang.org/docs/handbook/jsx.html#json)
- [Tutoriel JSON pour débutants](https://www.w3schools.com/js/js_json_intro.asp)
- [Vidéo YouTube sur l'utilisation de JSON dans TypeScript](https://www.youtube.com/watch?v=WCjmYL8rn9U)
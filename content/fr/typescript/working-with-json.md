---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) est un format pour échanger des données. Les devs l'utilisent car il est léger, facile à lire et à écrire, et bien intégré dans JavaScript et TypeScript.

## How to:
Pour travailler avec JSON en TypeScript, il faut d'abord comprendre comment parser (analyser) et stringifier (convertir en chaîne de caractères).

```typescript
// Convertir une chaîne JSON en objet TypeScript
let jsonData: string = '{"name": "Jean", "age": 30}';
let user: { name: string; age: number } = JSON.parse(jsonData);
console.log(user.name); // Affiche "Jean"

// Convertir un objet TypeScript en chaîne JSON
let userObject: { name: string; age: number } = { name: "Marie", age: 25 };
let jsonString: string = JSON.stringify(userObject);
console.log(jsonString); // Affiche '{"name":"Marie","age":25}'
```

## Deep Dive
JSON existe depuis les années 2000 et est devenu le standard de fait pour les API et les configurations. XML était l'alternative avant, mais JSON l'a largement dépassé en popularité. En TypeScript, la sérialisation et la désérialisation sont gérées nativement par `JSON.parse` et `JSON.stringify`. La typage statique de TypeScript aide à gérer la structure des données en JSON.

## See Also
- Documentation JSON MDN: [MDN JSON](https://developer.mozilla.org/fr/docs/Learn/JavaScript/Objects/JSON)
- Comparaison JSON vs XML: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
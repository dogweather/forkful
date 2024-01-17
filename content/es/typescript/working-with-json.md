---
title:                "Trabajando con json"
html_title:           "TypeScript: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con JSON (JavaScript Object Notation) es una forma de estructurar y almacenar datos en un formato fácilmente legible por computadoras. Los programadores usan JSON para transferir y almacenar datos, ya que es un formato ligero y ampliamente compatible con diferentes lenguajes de programación.

## Cómo hacerlo:
Para trabajar con JSON en TypeScript, es necesario importar el módulo 'json'. A continuación, puedes utilizar la función 'stringify' para convertir un objeto en una cadena JSON y 'parse' para convertir una cadena JSON en un objeto. A continuación se presentan ejemplos de cómo utilizar estas funciones:

```TypeScript
// convirtiendo un objeto en una cadena JSON
import * as json from 'json';
let miObjeto = { nombre: 'Juan', edad: 26 };
let miObjetoJSON = json.stringify(miObjeto);
console.log(miObjetoJSON);
```
Salida: `{"nombre":"Juan","edad":26}`

```TypeScript
// convirtiendo una cadena JSON en un objeto
import * as json from 'json';
let miStringJSON = '{"nombre":"Maria","edad":30}';
let miObjeto = json.parse(miStringJSON);
console.log(miObjeto.nombre);
console.log(miObjeto.edad);
```
Salida:
`Maria`
`30`

## Inmersión profunda:
JSON fue creado por Douglas Crockford en 2001 como una alternativa al formato XML para el intercambio de datos en la web. A lo largo de los años, ha ganado popularidad y se ha convertido en un estándar en la industria. Además, hay diferentes bibliotecas de codificación y decodificación de JSON disponibles en muchos lenguajes de programación, lo que lo hace muy versátil y fácil de implementar.

## Ver también:
- Documentación oficial de JSON en TypeScript: https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html
- Documentación oficial de JSON: https://www.json.org/json-es.html
- Tutoriales de programación con TypeScript: https://www.udemy.com/course/aprendiendo-typescript/
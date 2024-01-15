---
title:                "Extrayendo subcadenas"
html_title:           "TypeScript: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una tarea común en la programación, ya sea para manipular datos de manera eficiente o para validar información en formularios. En TypeScript, esta tarea se vuelve aún más sencilla gracias a sus métodos incorporados.

## Cómo hacerlo

```TypeScript
// Crear una cadena de ejemplo
let cadena: string = "Hola Mundo";

// Extraer una subcadena de la posición 5 hasta el final
let subcadena = cadena.substring(5);
console.log(subcadena); // Salida: "Mundo"

// Extraer una subcadena de la posición 2 hasta la 7
let otraSubcadena = cadena.substring(2, 7);
console.log(otraSubcadena); // Salida: "la Mu"
```

Para extraer subcadenas en TypeScript, podemos utilizar el método `substring()` que acepta dos parámetros: la posición de inicio y la posición de final (opcional). Esto nos permite seleccionar una parte específica de una cadena de manera muy sencilla.

## Profundizando

Además del método `substring()`, TypeScript también ofrece otras formas de extraer subcadenas:

- `slice(start?: number, end?: number)` nos permite seleccionar una porción de una cadena, similar a `substring()` pero también acepta valores negativos para contar desde el final.
- `substr(from: number, length?: number)` nos permite seleccionar una subcadena iniciando en una posición y de una longitud determinadas.

Es importante tener en cuenta que en TypeScript, al igual que en JavaScript, las cadenas son inmutables, lo que significa que una vez que se crea una cadena, no se pueden modificar sus caracteres individuales. Por lo tanto, cada método de extracción de subcadenas devuelve una nueva cadena en lugar de modificar la original.

## Ver también

- [Documentación oficial de TypeScript sobre métodos de manipulación de cadenas](https://www.typescriptlang.org/docs/handbook/destructuring.html#substring)
- [Artículo en español sobre operaciones comunes con cadenas en TypeScript](https://www.arquitecturajava.com/operaciones-basicas-manipulacion-cadenas-typescript/)
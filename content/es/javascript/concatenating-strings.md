---
title:                "Concatenación de cadenas de texto"
date:                  2024-01-20T17:35:02.532994-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Concatenar cadenas significa unir dos o más textos en uno. Los programadores lo hacen para manipular y combinar información textual de manera dinámica y eficiente.

## Cómo hacerlo:

```javascript
// Concatenación con el operador +
let saludo = "Hola, " + "mundo!";
console.log(saludo); // "Hola, mundo!"

// Concatenación con el método concat()
let saludoCompleto = "¡Bienvenido a ".concat("JavaScript!");
console.log(saludoCompleto); // "¡Bienvenido a JavaScript!"

// Usando template literals (Plantillas de cadena)
let nombre = "María";
let saludoPersonalizado = `Hola, ${nombre}!`;
console.log(saludoPersonalizado); // "Hola, María!"
```

## Detalles Profundos:

Históricamente, la concatenación de cadenas era una tarea básica, pero con varias maneras de hacerla. Originalmente, el único método era utilizar el operador `+`, que sigue siendo muy utilizado por su simplicidad.

Con el tiempo, se introdujo el método `concat()`, que puede ser más legible y expresivo. Aunque `concat()` parece más específico, no mejora sustancialmente el rendimiento y es menos común que el operador `+`.

Los template literals o plantillas de cadena son una de las adiciones más recientes a JavaScript (introducidas en ES6) y permiten una sintaxis más limpia y capacidad para interpolar variables y expresiones dentro de cadenas.

En cuanto a rendimiento, las operaciones de concatenación simples son bastante eficientes en los motores JavaScript modernos; sin embargo, concatenar un gran número de cadenas puede ser más eficiente usando `Array.join()` debido a consideraciones de memoria y rendimiento.

La elección del método de concatenación dependerá del contexto y la preferencia personal; sin embargo, los template literals son a menudo la opción más limpia y moderna para la inclusión de variables y expresiones dentro de las cadenas.

## Ver También:

- Documentación de MDN sobre template strings: [MDN Template Literals](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Template_literals)
- Información sobre concat(): [MDN String.prototype.concat()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
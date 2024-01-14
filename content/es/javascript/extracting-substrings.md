---
title:                "Javascript: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer subcadenas en programación Javascript

La extracción de subcadenas, también conocida como subcadena, es una técnica común en programación que implica seleccionar una parte de una cadena de texto más grande. Hay varias razones por las que alguien podría querer extraer subcadenas en Javascript, incluyendo la manipulación de datos, la validación de información o la creación de una interfaz de usuario dinámica.

## Cómo hacerlo

Para extraer subcadenas en Javascript, se utiliza el método `substring ()`, que toma dos parámetros: el índice de inicio y el índice final de la subcadena deseada. Aquí hay un ejemplo de cómo extraer una subcadena de una cadena de texto:

```Javascript
let texto = "Hola, ¡bienvenido a mi blog!";
let subcadena = texto.substring(6, 15);
console.log(subcadena); // salida: ¡bienvenido
```

En este ejemplo, el índice de inicio es 6 porque "¡bienvenido" comienza en la sexta posición en la cadena de texto y el índice final es 15 porque la subcadena debe terminar antes de "a" en "blog".

## Profundizando

Además del método `substring ()`, también se puede utilizar el método `slice ()` para extraer subcadenas en Javascript. La diferencia entre estos dos métodos es que `slice ()` toma un índice negativo como parámetro, lo que le permite comenzar a contar desde el final de la cadena.

También es posible utilizar un solo parámetro con `substring ()` o `slice ()`, lo que significa que la subcadena comenzará desde el índice proporcionado hasta el final de la cadena.

En caso de necesitar una subcadena específica de una cadena de texto basada en un patrón, se puede utilizar el método `match ()` junto con una expresión regular para extraerla.

## Ver también

- Tutorial de Javascript para principiantes: https://www.w3schools.com/js/
- Documentación oficial de Javascript: https://developer.mozilla.org/es/docs/Web/JavaScript
- Ejemplos prácticos de extracción de subcadenas en Javascript: https://www.programiz.com/javascript/examples/?subTopicId=substring
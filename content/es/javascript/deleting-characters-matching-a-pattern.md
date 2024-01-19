---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Eliminar caracteres coincidentes con un patrón es un aspecto integral de la manipulación de cadenas en JavaScript. Esto permite a los programadores limpiar datos, reformatar texto o extraer información específica de una cadena.

## Cómo hacerlo:

En JavaScript, puedes usar la función `replace()` junto con una expresión regular para eliminar caracteres que coinciden con un patrón específico.

```Javascript 
let cadena = "¡JavaScript es asombroso!";
let cadenaLimpia = cadena.replace(/asombroso/gi, "");
console.log(cadenaLimpia);  // ¡JavaScript es !
```
En este ejemplo, el método `replace()` está eliminando 'asombroso'. Puedes cambiar 'asombroso' para que coincida con el patrón que deseas eliminar.

## Análisis Profundo

Eliminando caracteres coincidentes con un patrón es un concepto que proviene de las expresiones regulares, ampliamente usadas en la programación desde los años 60. Otra forma de hacerlo en JavaScript sería usar el método `split()` seguido de `join()`, pero esto puede ser menos eficiente en ciertos casos.

```Javascript
let cadena2 = "¡JavaScript es increíble!";
let cadenaLimpia2 = cadena2.split('increíble').join('');
console.log(cadenaLimpia2);  // ¡JavaScript es !
```
En términos de rendimiento, si la cadena es extremadamente larga, `replace()` puede ser más rápido ya que no implica la creación de un array adicional. Sin embargo, `split().join()` puede ser más adecuado si necesitas trabajar con cada fragmento individualmente.

## Ver También

Para más información sobre expresiones regulares en JavaScript, visita [MDN web docs](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions).

Para más detalles sobre manipulación de cadenas en JavaScript, consulta [w3schools](https://www.w3schools.com/js/js_string_methods.asp).
---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Javascript: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas es una operación común en la programación que nos permite normalizar y manipular datos de manera eficiente. Este proceso es especialmente útil cuando estamos trabajando con entradas del usuario que pueden tener diferentes formatos de mayúsculas y minúsculas.

## Cómo hacerlo

Usar el método `toLowerCase()` es la forma más sencilla de convertir una cadena de texto a minúsculas en JavaScript. Veamos un ejemplo:

```Javascript
let texto = "Hola Mundo";
let textoEnMinusculas = texto.toLowerCase();

console.log(textoEnMinusculas);
// Output: hola mundo
```

En este caso, hemos creado una variable `texto` con la cadena de texto "Hola Mundo". Luego, usando el método `toLowerCase()`, hemos creado una nueva variable `textoEnMinusculas` que contiene la misma cadena, pero en minúsculas. Finalmente, imprimimos el resultado en la consola y obtenemos "hola mundo".

El método `toLowerCase()` también nos permite manipular cadenas con caracteres acentuados. Por ejemplo:

```Javascript
let texto = "Árbol Frutal";
let textoEnMinusculas = texto.toLowerCase();

console.log(textoEnMinusculas);
// Output: árbol frutal
```

En este caso, podemos ver que incluso el carácter acentuado `Á` ha sido convertido a su equivalente en minúsculas `á`.

## Profundizando

JavaScript es un lenguaje de programación "dinámico", lo que significa que el tipo de datos de una variable puede cambiar durante la ejecución del programa. Esto es importante tenerlo en cuenta a la hora de convertir una cadena de texto a minúsculas. Veamos un ejemplo:

```Javascript
let numero = 123;
console.log(typeof(numero));
// Output: number

let texto = "Ejemplo";
texto = texto.toLowerCase();
console.log(typeof(texto));
// Output: string
```

En este caso, creamos una variable `numero` con un valor numérico y vemos que su tipo de datos es `number`. Luego, asignamos a esa misma variable el método `toLowerCase()` que convierte a su tipo de datos en `string`.

Otra característica importante a tener en cuenta es que el método `toLowerCase()` no modifica la cadena original, sino que crea una nueva cadena con los cambios aplicados. Por lo tanto, si queremos guardar la cadena en minúsculas, debemos asignarla a una nueva variable.

En resumen, convertir una cadena de texto a minúsculas en JavaScript es una operación sencilla pero útil que nos permite normalizar y manipular datos de manera eficiente. Aprovechemos este método para mejorar nuestras habilidades de programación.

## Ver también

- Documentación del método `toLowerCase()` en MDN: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Uso del método `toLowerCase()` en ejemplos reales: https://www.w3schools.com/jsref/jsref_tolowercase.asp
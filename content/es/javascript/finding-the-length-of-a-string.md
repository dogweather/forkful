---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Encontrar la longitud de un string significa calcular el número de caracteres en esa cadena de texto. Los programadores a menudo necesitan saber cuántos caracteres hay para funciones como la validación de entrada, la manipulación de texto y más.

## ¿Cómo se hace?

En JavaScript, puedes encontrar la longitud de un string usando el método `.length`. Aquí te dejo un ejemplo.

``` Javascript
let frase = '¡Hola, Mundo!';
console.log(frase.length); // devuelve: 13
```

En este código, `frase.length` devuelve `13` porque '¡Hola, Mundo!' tiene 13 caracteres, incluyendo comas y espacios.

## Un análisis más profundo

Desde los primeros días del lenguaje JavaScript, siempre ha habido una forma de obtener la longitud de un string. Sólo necesitas llamar a la propiedad `.length` de tu string.

De manera alternativa, podrías convertir la cadena de texto a un array usando el método `.split('')` y luego obtener su longitud, pero esto es innecesariamente complicado y lento.

Un detalle de implementación interesante es que la propiedad `.length` no recuenta los caracteres cada vez que se la llama. En vez de eso, JavaScript almacena la longitud de la cadena de texto cuando se crea, y simplemente devuelve este valor cuando accedes a `.length`. Esto significa que obtener la longitud de un string es rápido, independientemente de su tamaño.

## Ver también

MDN (Mozzila Developer Network) proporciona una excelente referencia sobre el método `.length` en JavaScript:

- [MDN - String.length](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/length)
---
title:                "Foro de programación: Encontrando la longitud de una cadena"
html_title:           "Javascript: Foro de programación: Encontrando la longitud de una cadena"
simple_title:         "Foro de programación: Encontrando la longitud de una cadena"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¡Descubre la longitud de una cadena con Javascript!

En la programación, a menudo necesitamos saber la cantidad de caracteres que hay en una cadena de texto. Ya sea para validar una entrada de usuario, manipular esa cadena o simplemente por curiosidad, saber la longitud de una cadena es una habilidad básica y útil en Javascript.

## ¿Por qué?

Saber la longitud de una cadena es esencial para muchas tareas de programación. Por ejemplo, puede ser necesario limitar la cantidad de caracteres que un usuario puede ingresar en un formulario, o dividir una cadena en partes iguales. Además, algunos algoritmos y funciones en Javascript se basan en la longitud de una cadena para realizar cálculos y operaciones.

## ¿Cómo hacerlo?

Afortunadamente, Javascript tiene una función integrada llamada `length` que nos permite obtener fácilmente la longitud de una cadena. Veamos un ejemplo de cómo usarla:

```javascript
let cadena = "Hola mundo";
console.log(cadena.length); // Output: 10
```

En este ejemplo, primero declaramos una variable `cadena` y le asignamos el valor de "Hola mundo". Luego, usamos la función `length` para obtener la longitud de la cadena y lo imprimimos en la consola. Como resultado, obtenemos el número 10, que es la cantidad de caracteres en la cadena.

También podemos usar la función `length` en una cadena vacía o una cadena que solo contenga espacios en blancos, lo que también nos devuelve un número:

```javascript
let cadenaVacia = "";
console.log(cadenaVacia.length); // Output: 0

let cadenaEspacios = "          ";
console.log(cadenaEspacios.length); // Output: 10
```

Pero, ¿qué pasa si queremos obtener la longitud de una cadena que incluye caracteres especiales o acentos? ¡No te preocupes! La función `length` cuenta todos los caracteres, incluidos los especiales y los acentos.

## Profundizando

En Javascript, todas las cadenas de texto tienen una propiedad llamada `length` que almacena la cantidad de caracteres. Esta propiedad es de solo lectura, lo que significa que no podemos cambiar su valor, solo acceder a él. Además, la función `length` solo funciona con cadenas de texto, es decir, no podemos usarla en otros tipos de datos como números o booleanos.

También es importante tener en cuenta que la función `length` no cuenta los espacios en blanco al principio o al final de una cadena. Esto significa que la cadena "  Hola " tendría una longitud de 4, aunque visualmente tenga 6 caracteres.

## Mira también

- [Documentación MDN de la función length en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Tutorial de Javascript: Cadena de texto](https://www.w3schools.com/js/js_strings.asp)
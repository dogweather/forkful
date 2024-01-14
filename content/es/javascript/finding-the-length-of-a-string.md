---
title:                "Javascript: Encontrando la longitud de una cadena"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo nos encontramos con la necesidad de encontrar la longitud de una cadena (string en inglés). Esto puede ser útil para realizar operaciones como la validación de datos, la manipulación de textos y mucho más. Aunque puede parecer una tarea simple, es importante entender por qué es importante y cómo podemos hacerlo de manera eficiente en Javascript.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Javascript, podemos usar el método integrado `length`. Este método devuelve la cantidad de caracteres en una cadena. Veamos un ejemplo de cómo usarlo:

```Javascript
let cadena = "¡Hola Mundo!";
console.log(cadena.length); //Salida: 11
```

En este ejemplo, creamos una variable `cadena` que contiene la frase "¡Hola Mundo!". Luego, utilizamos el método `length` para obtener su longitud y la mostramos en la consola. Como pueden ver, la salida es 11, ya que hay 11 caracteres en la cadena incluyendo los espacios.

También podemos usar este método en variables que contengan números, pero debemos tener en cuenta que solo nos dará la cantidad de dígitos, no la cantidad de números. Por ejemplo:

```Javascript
let numero = 12435;
console.log(numero.length); //Salida: 5
```

## Profundizando

El método `length` es uno de los muchos métodos que existen en Javascript para trabajar con cadenas. Algunos otros métodos útiles son `indexOf`, `slice`, `substring`, entre otros. Es importante familiarizarse con ellos para poder manipular y obtener información de las cadenas de manera más efectiva. Además, también es importante considerar que la longitud devuelta por el método `length` no solo cuenta los caracteres visibles, sino también los espacios en blanco, tabulaciones y saltos de línea.

## Ver también

- [Método Length en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Manipulación de cadenas en Javascript](https://www.w3schools.com/js/js_string_methods.asp)
- [Validación de datos en Javascript](https://www.geeksforgeeks.org/javascript-data-validation/)
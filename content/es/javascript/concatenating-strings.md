---
title:                "Uniendo cadenas de texto"
html_title:           "Javascript: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por Qué

Concatenar strings, o unir diferentes cadenas de texto en una sola, es una tarea común y útil en la programación en Javascript. Esta técnica nos permite crear mensajes personalizados, generar reportes y manipular datos de manera eficiente.

## Cómo Hacerlo

Para concatenar strings en Javascript, podemos usar el operador `+` o el método `.concat()`.

### Usando el operador `+`

```Javascript
let nombre = "Juan";
let edad = 25;
let mensaje = "Hola, mi nombre es " + nombre + " y tengo " + edad + " años.";
console.log(mensaje);
// Salida: Hola, mi nombre es Juan y tengo 25 años.
```

En este ejemplo, hemos utilizado el operador `+` para unir las diferentes cadenas de texto y variables en una sola, creando un mensaje personalizado.

### Usando el método `.concat()`

```Javascript
let nombre = "Maria";
let apellido = "García";
let mensaje = nombre.concat(" ", apellido);
console.log(mensaje);
// Salida: Maria García
```

En este caso, hemos utilizado el método `.concat()` para unir dos strings en una sola, incluyendo un espacio en blanco entre ellas para que el nombre y apellido se vean correctamente separados.

## Inmersión Profunda

Aunque el uso del operador `+` y el método `.concat()` son las formas más comunes de concatenar strings en Javascript, existen otras técnicas y funciones que nos pueden ayudar a realizar esta tarea.

Por ejemplo, el método `.join()` puede ser utilizado para unir elementos de un array en una sola string. También podemos utilizar templates literales, haciendo uso de las comillas invertidas (`) en lugar de las comillas simples o dobles, para incluir variables y expresiones en una sola string de manera más legible.

En general, es importante tener en cuenta que al concatenar strings, siempre debemos asegurarnos de que los tipos de datos sean compatibles para evitar errores en nuestra aplicación.

## Ver También

- [Documentación oficial de Javascript sobre la concatenación de strings](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Ejemplos prácticos de concatenación de strings en Javascript](https://www.codegrepper.com/code-examples/javascript/how+to+concatenate+strings+in+javascript)
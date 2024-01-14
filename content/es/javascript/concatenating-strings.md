---
title:                "Javascript: Uniendo cadenas de caracteres"
simple_title:         "Uniendo cadenas de caracteres"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

En la programación de Javascript, a menudo necesitamos combinar diferentes cadenas de texto para crear mensajes o información más compleja. La concatenación de cadenas es una técnica útil que nos permite unir diferentes cadenas de texto en una sola. 

## Cómo hacerlo

La concatenación de cadenas en Javascript es bastante sencilla. Podemos utilizar el operador "+" para unir dos o más cadenas de texto. Por ejemplo:

```Javascript
let nombre = "Sofía";
let apellido = "González";
let nombreCompleto = nombre + " " + apellido;

console.log(nombreCompleto); // Output: Sofía González
```

En este ejemplo, utilizamos el operador "+" para concatenar las variables "nombre" y "apellido" en la variable "nombreCompleto". Al imprimir esta variable en la consola, obtenemos como resultado la cadena "Sofía González".

También podemos concatenar cadenas con valores numéricos. En ese caso, los valores numéricos se convierten en cadenas antes de ser concatenados. Por ejemplo:

```Javascript
let precio = "$";
let monto = 50.99;
let precioFinal = precio + monto;

console.log(precioFinal); // Output: $50.99
```

En este caso, la variable "monto" se convierte en una cadena antes de ser concatenada con la variable "precio".

## Profundizando

La concatenación de cadenas puede ser muy útil cuando necesitamos crear mensajes dinámicos en nuestra aplicación. Además del operador "+", también podemos utilizar la función "concat()" para concatenar cadenas. Ambas opciones son igualmente válidas, pero a menudo se prefiere el uso del operador "+" por ser más sencillo y legible.

Además, es importante tener en cuenta que al concatenar cadenas, podemos utilizar diferentes tipos de comillas (simples, dobles o invertidas) dentro de la cadena sin ningún problema. Esto nos permite utilizar comillas en medio de un texto sin interrumpir la concatenación.

## Ver también

Si quieres seguir aprendiendo sobre cadenas en Javascript, te recomendamos estos recursos:

- [Función concat en W3Schools](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [Manipulación de cadenas en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String)
---
title:    "Javascript: Encontrando la longitud de una cadena"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, es común encontrarse con la necesidad de conocer la longitud de una cadena de texto. Ya sea para validar la entrada del usuario o para realizar operaciones específicas, saber la cantidad de caracteres en una cadena puede ser muy útil.

## Cómo hacerlo

Hay diferentes formas de encontrar la longitud de una cadena en JavaScript. Una de las formas más sencillas es utilizando el método `length`. Este método se puede aplicar a cualquier cadena de texto y devuelve el número de caracteres que contiene. Veamos un ejemplo:

```Javascript
let miCadena = "¡Hola, mundo!";
console.log(miCadena.length); // Output: 13
```

También se puede utilizar la propiedad `length` de un objeto String. Por ejemplo:

```Javascript
let miOtraCadena = new String("¡Hola!");
console.log(miOtraCadena.length); // Output: 5
```

Es importante notar que tanto el método `length` como la propiedad `length` cuentan los espacios en blanco como caracteres, por lo que no es necesario realizar ningún tipo de manipulación adicional para obtener la longitud correcta de una cadena.

## Profundizando

Al utilizar el método `length` o la propiedad `length`, se está accediendo al valor de una propiedad interna del objeto String. Esta propiedad se llama "length" y contiene el número de caracteres de la cadena. Es importante saber que esta propiedad es de solo lectura, lo que significa que no se puede asignar un valor a ella.

Por otro lado, es importante tener en cuenta que existen diferencias entre caracteres y bytes. Mientras que en la mayoría de los casos un carácter se corresponde con un byte, hay ciertas excepciones en las que un carácter puede ocupar más de un byte, lo que afectará el valor de la propiedad `length` en estas situaciones.

## Ver también

- [Documentación de String.length en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Método length en W3Schools](https://www.w3schools.com/jsref/jsref_length_string.asp)
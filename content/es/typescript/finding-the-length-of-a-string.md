---
title:                "TypeScript: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos saber la longitud de una cadena de texto. Esto puede ser útil al validar entradas de usuarios, crear bucles y en muchas otras situaciones. A continuación, te explicaremos cómo encontrar la longitud de una cadena en TypeScript.

## Cómo hacerlo

```TypeScript
const texto = "¡Hola, amigos!";
console.log(texto.length); // output: 14
```

En el código anterior, hemos declarado una variable llamada "texto" que contiene una cadena de texto. Luego, utilizamos la propiedad "length" para encontrar su longitud y lo imprimimos en la consola. La propiedad "length" es una función integrada en TypeScript que nos permite obtener la cantidad de caracteres en una cadena.

También podemos utilizar un bucle para encontrar la longitud de una cadena:

```TypeScript
const texto = "¡Hola, amigos!";
let longitud = 0;
for (let i = 0; i < texto.length; i++) {
    longitud++;
}
console.log(longitud); // output: 14
```

En este ejemplo, inicializamos una variable llamada "longitud" con un valor de cero. Luego, utilizamos un bucle para recorrer cada carácter de la cadena y aumentar la longitud en 1 en cada iteración. Al final, la variable "longitud" contendrá el valor total de la longitud de la cadena.

## Profundizando

Es importante destacar que la función "length" cuenta todos los caracteres en la cadena, incluyendo espacios en blanco, comas y signos de puntuación. También cuenta los caracteres especiales y emojis como un solo carácter. Por ejemplo, si tenemos una cadena de texto que contiene un emoji, su longitud sería de 2 en lugar de 1. Además, la propiedad "length" es de solo lectura, lo que significa que no podemos cambiar la longitud de una cadena modificando esta propiedad.

## Ver también

- [Documentación oficial de TypeScript sobre strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Ejemplos de propiedades de strings en TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [Artículo sobre métodos de strings en TypeScript](https://www.freecodecamp.org/news/typescript-string-how-to-use/)
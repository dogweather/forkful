---
title:    "TypeScript: Extrayendo subcadenas"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## ¿Por qué? 

Extraer subcadenas es una técnica útil en la programación TypeScript y puede ayudar a los desarrolladores a manipular y trabajar con cadenas de caracteres de manera eficiente. En lugar de tener que trabajar con una cadena completa, podemos extraer solo la parte que necesitamos, ahorrando tiempo y esfuerzo en el desarrollo de aplicaciones.

## Cómo hacerlo

Para extraer subcadenas en TypeScript, podemos utilizar el método `substring()` que viene incorporado en la clase `String`. Este método acepta dos parámetros: el índice inicial donde queremos comenzar la subcadena y el índice final donde queremos terminarla. Veamos un ejemplo:

```TypeScript
let cadena = "Hola Mundo";
let subcadena = cadena.substring(5, 9);

console.log(subcadena); // Output: Mundo
```

En este ejemplo, especificamos que queremos comenzar la subcadena en el quinto índice, que corresponde a la letra "M" en "Mundo", y terminar en el noveno índice, que corresponde a la letra "o" en "Mundo". El resultado es la subcadena "Mundo", que se imprime en la consola.

Podemos utilizar también un solo parámetro en el método `substring()`, en cuyo caso, la subcadena se extraerá desde ese índice hasta el final de la cadena, como se muestra a continuación:

```TypeScript
let cadena = "Hola Mundo";
let subcadena = cadena.substring(5);

console.log(subcadena); // Output: Mundo
```

Si no especificamos ningún parámetro, la subcadena será una copia idéntica de la cadena original.

## Profundizando

El método `substring()` también permite manejar índices negativos, lo que nos permite extraer subcadenas contando desde el final de la cadena hacia el comienzo. Por ejemplo:

```TypeScript
let cadena = "Hola Mundo";
let subcadena = cadena.substring(-5);

console.log(subcadena); // Output: Mundo
```

En este caso, el parámetro -5 corresponde al quinto índice contando desde el final de la cadena, que también es la letra "M" en "Mundo".

También podemos utilizar el método `substring()` para extraer múltiples subcadenas de una sola cadena. Para eso, podemos envolver el método en un bucle y utilizar variables para almacenar los índices y las subcadenas resultantes.

## Ver También

- [Documentación oficial de TypeScript sobre `substring()`](https://www.typescriptlang.org/docs/handbook/basic-types.html#substring) 
- [Tutorial de W3Schools sobre `substring()` en TypeScript](https://www.w3schools.com/jsref/jsref_substring.asp)
- [Artículo de Medium sobre las diferentes formas de trabajar con subcadenas en TypeScript](https://medium.com/@wesharehoodies/typescript-split-a-string-into-two-parts-dde1180e9918)
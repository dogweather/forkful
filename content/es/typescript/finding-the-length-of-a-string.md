---
title:                "Encontrando la longitud de una cadena"
html_title:           "TypeScript: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, es común trabajar con cadenas de texto y muchas veces necesitamos saber cuál es su longitud. Saber la longitud de una cadena puede ser útil para realizar operaciones específicas, como validar una entrada de usuario o recorrer una cadena para buscar ciertos caracteres. En TypeScript, hay una función incorporada que nos permite obtener la longitud de una cadena y en este artículo te mostraré cómo utilizarla.

## Cómo hacerlo

Para obtener la longitud de una cadena en TypeScript, utilizamos la función `length` en la variable de la cadena. Por ejemplo, si tenemos una cadena `mensaje` que contiene "Hola Mundo", podemos obtener su longitud de la siguiente manera:

```TypeScript
let mensaje: string = "Hola Mundo";
console.log(mensaje.length); // output: 10
```

Como se puede ver, la propiedad `length` devuelve un número que representa la longitud de la cadena.

Otra forma de obtener la longitud de una cadena es usando la función `new String()` en lugar de una variable definida previamente. Por ejemplo:

```TypeScript
let longitudCadena: number = new String("Esto es una cadena").length; 
console.log(longitudCadena); // output: 20
```

También puedes utilizar esta función directamente en la consola del navegador para obtener la longitud de una cadena específica. Simplemente abre la consola de desarrollador en tu navegador y escribe:

```TypeScript
"Probando".length; // output: 8
```

## Profundizando

Para entender mejor cómo funciona la función `length`, es importante conocer cómo TypeScript maneja las cadenas de texto. En TypeScript, las cadenas de texto son en realidad objetos y la propiedad `length` es una de las propiedades de estos objetos.

Además de `length`, hay otras propiedades y métodos que se pueden utilizar en las cadenas de texto, como `charAt()` para obtener un carácter específico en una posición determinada, `indexOf()` para buscar la posición de un carácter o un conjunto de caracteres y `split()` para dividir una cadena en un array de subcadenas. Puedes aprender más sobre estas propiedades y métodos en la documentación oficial de TypeScript.

En resumen, la función `length` es una forma rápida y sencilla de obtener la longitud de una cadena de texto en TypeScript. Conociendo esta función y otras propiedades y métodos de las cadenas, podrás manipular y trabajar de forma más eficiente con cadenas de texto en tus proyectos.

## Ver también

- [Documentación oficial de TypeScript sobre cadenas de texto](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Ejemplos de uso de la función length en TypeScript](https://www.geeksforgeeks.org/typescript-string-length-property/)
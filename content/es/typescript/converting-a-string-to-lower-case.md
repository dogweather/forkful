---
title:    "TypeScript: Convirtiendo una cadena a minúsculas"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué
Antes de adentrarnos en cómo convertir una cadena de texto a minúsculas en TypeScript, es importante entender por qué esto es útil. La conversión de una cadena de texto a minúsculas es una tarea común en la programación, ya que nos permite estandarizar el formato de una cadena y facilitar su manipulación y comparación. Con TypeScript, podemos hacer esto de manera sencilla y eficiente.

## Cómo hacerlo
Para convertir una cadena de texto a minúsculas en TypeScript, podemos utilizar el método `toLowerCase()`, que devuelve una nueva cadena con todos los caracteres en minúsculas. Veamos un ejemplo:

```TypeScript
const texto = "HOla MUndo";
const textoMiniscula = texto.toLowerCase();
console.log(textoMiniscula); // "hola mundo"
```

En este código, primero declaramos una variable `texto` y le asignamos una cadena con letras mayúsculas y minúsculas. Luego, utilizamos el método `toLowerCase()` para convertir esa cadena a minúsculas y lo almacenamos en una nueva variable `textoMiniscula`. Por último, imprimimos el resultado en la consola, que será la cadena en minúsculas.

También podemos utilizar el método `toLowerCase()` directamente sobre la cadena que queremos convertir, sin necesidad de almacenarla en una variable nueva:

```TypeScript
const resultado = "Mi texto en MAYÚSCULAS".toLowerCase();
console.log(resultado); // "mi texto en mayúsculas"
```

## Profundizando
Para entender mejor cómo funciona la conversión de cadenas a minúsculas en TypeScript, debemos conocer cómo se manejan las cadenas de texto en este lenguaje. TypeScript utiliza el tipo `string` para representar cadenas de texto, y algunas de sus propiedades y métodos están presentes en otros lenguajes de programación.

El método `toLowerCase()` utiliza los estándares Unicode para determinar qué caracteres deben ser convertidos a minúsculas. Esto significa que, dependiendo del idioma en el que estemos trabajando, pueden aparecer diferencias en la conversión de ciertos caracteres.

Además, es importante tener en cuenta que el método `toLowerCase()` no afecta a los caracteres especiales, como acentos o diéresis. Estos seguirán siendo convertidos a minúsculas en su forma original. Por ejemplo:

```TypeScript
const frase = "Qué lindo día en España!";
const fraseMiniscula = frase.toLowerCase();
console.log(fraseMiniscula); // "qué lindo día en españa!"
```

Podemos ver que los caracteres acentuados no han sido eliminados, sino que se han mantenido en su forma original.

## Ver también
- [Documentación oficial de TypeScript sobre el tipo `string`](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Métodos de cadena en TypeScript](https://www.w3schools.com/js/js_string_methods.asp)
- [Artículo de programación sobre el uso de métodos en TypeScript](https://medium.com/@robertcooper_rc/an-overview-of-javascript-testing-in-2019-264e19514d0a)
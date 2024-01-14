---
title:                "TypeScript: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué convertir una cadena de texto a minúsculas es importante en TypeScript

A veces, en la programación, nos encontramos con la necesidad de comparar cadenas de texto, ya sea para buscar un determinado patrón o para validar una entrada de usuario. Sin embargo, las cadenas de texto pueden contener mayúsculas y minúsculas, lo que puede dificultar la comparación exacta. Por eso, es importante saber cómo convertir una cadena de texto a minúsculas en TypeScript.

## Cómo convertir una cadena de texto a minúsculas en TypeScript

Para convertir una cadena de texto a minúsculas en TypeScript, podemos utilizar el método `toLowerCase()`. Este método se aplica a una cadena de texto y devuelve una nueva cadena con todos los caracteres en minúsculas. Veamos un ejemplo:

```TypeScript
let str: string = "Hola Mundo";

console.log(str.toLowerCase()); // salida: hola mundo
```

En este ejemplo, primero declaramos una variable `str` con la cadena de texto "Hola Mundo". Luego, utilizamos el método `toLowerCase()` para convertir esta cadena a minúsculas y la imprimimos en la consola.

## Deep Dive: Más información sobre la conversión de cadenas de texto a minúsculas

Es importante tener en cuenta que el método `toLowerCase()` solo convierte los caracteres que tengan una representación en minúsculas. Por ejemplo, la letra `Á` no tiene una letra correspondiente en minúsculas, por lo que no se convertirá. Además, este método no modificará la cadena original, sino que devolverá una nueva cadena con los cambios realizados.

Otra forma de convertir una cadena de texto a minúsculas es utilizando el operador de asignación `+=` junto con el método `toLowerCase()`. Este operador se puede utilizar para concatenar una cadena a otra, por lo que podemos utilizarlo para asignar la cadena original en minúsculas a una nueva variable. Veamos un ejemplo:

```TypeScript
let originalStr: string = "HOLA MUNDO";
let lowerCaseStr: string = "";

lowerCaseStr += originalStr.toLowerCase(); // lowerCaseStr = "hola mundo"
```

En este caso, utilizamos el operador `+=` para asignar la cadena original en minúsculas a la variable `lowerCaseStr`.

## Ver también

- [Documentación oficial de TypeScript: Métodos de cadena](https://www.typescriptlang.org/docs/handbook/2/types-for-javascript-users.html#string-representation-methods)
- [Tutorial de TypeScript: Manipulación de cadenas de texto](https://www.tutorialsteacher.com/typescript/typescript-string)
- [Convertir todas las letras a minúsculas en JavaScript](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
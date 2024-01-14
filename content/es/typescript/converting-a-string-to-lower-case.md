---
title:                "TypeScript: Convirtiendo una cadena a minúsculas"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas en TypeScript?

La conversión de cadenas a minúsculas puede ser útil en situaciones donde se necesite comparar cadenas de texto sin importar si están en mayúsculas o minúsculas. También puede facilitar la búsqueda y manipulación de texto en un programa.

## Cómo hacerlo en TypeScript

La forma más sencilla de convertir una cadena a minúsculas en TypeScript es utilizando el método `toLowerCase()`. Este método toma una cadena y devuelve una nueva cadena con todos los caracteres convertidos a minúsculas. Veamos un ejemplo:

```TypeScript
let cadena = "HOLA MUNDO";
let cadenaEnMinusculas = cadena.toLowerCase();
console.log(cadenaEnMinusculas);
```

El resultado de este código sería `hola mundo`.

Otra forma de hacerlo es utilizando la función `toLocaleLowerCase()`, que tiene la ventaja de ser más flexible, ya que permite especificar un idioma para la conversión. Por ejemplo:

```TypeScript
let cadena = "HOLA MUNDO";
let cadenaEnMinusculas = cadena.toLocaleLowerCase('es');
console.log(cadenaEnMinusculas);
```

El resultado en este caso también sería `hola mundo`, pero utilizando las reglas de ortografía del español.

## Profundizando en la conversión de cadenas a minúsculas

Es importante tener en cuenta que la conversión de cadenas a minúsculas puede variar dependiendo del idioma y las reglas de ortografía. Por ejemplo, en turco la letra "I" mayúscula se convierte a "ı" minúscula, mientras que en español se convierte a "i".

Además, es importante tener en cuenta que la conversión a minúsculas no solo afecta a las letras, sino también a los símbolos o caracteres especiales. Por ejemplo, la letra "Ñ" en español se convierte a "ñ" en minúsculas.

## Ver también

- [Método toLowerCase() en la documentación de TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#to-lower-case-and-to-upper-case-string-methods)
- [Función toLocaleLowerCase() en la documentación de TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-5.html#better-support-for-non-english-lower-casing-in-to-string)
- [Artículo sobre cómo funciona la conversión de cadenas a minúsculas en diferentes idiomas](https://hacks.mozilla.org/2018/03/es2018s-string-prototype-tolocalelowercase-is-a-gem/)
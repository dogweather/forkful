---
title:                "TypeScript: Concatenando cadenas"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué: 

Al escribir programas en TypeScript, a menudo nos encontramos con la necesidad de combinar o unir diferentes cadenas de texto. Esto puede ser útil para mostrar información al usuario, construir URLs dinámicas o simplemente para concatenar diferentes valores en una sola cadena. Aprender a concatenar cadenas en TypeScript es una habilidad importante para cualquier programador.

## Cómo hacerlo: 

La concatenación de cadenas es una operación sencilla en TypeScript. Se realiza utilizando el operador "+" o aprovechando el método "concat" disponible en las cadenas. Veamos algunos ejemplos:

```TypeScript
// Concatenación con el operador "+"
let nombre = "Juan";
let saludo = "Hola, mi nombre es " + nombre;
console.log(saludo);

// El resultado en la consola será:
// Hola, mi nombre es Juan

// Utilizando el método "concat"
let apellido = "García";
let nombreCompleto = nombre.concat(" ", apellido);
console.log(nombreCompleto);

// El resultado en la consola será:
// Juan García
```

Como se puede ver en los ejemplos anteriores, tanto el operador "+" como el método "concat" realizan la unión de dos o más cadenas en una sola. Además, se pueden combinar variables y texto estático para crear cadenas dinámicas.

## Detalles de la concatenación:

Es importante tener en cuenta que en TypeScript, al ser un lenguaje fuertemente tipado, no se puede concatenar directamente una cadena con otros tipos de datos, como números o booleanos. En estos casos, es necesario convertir el valor a cadena utilizando el método "toString" o utilizando plantillas de cadena (string templates).

Además, es posible concatenar más de dos cadenas utilizando cualquiera de los métodos mencionados anteriormente en sucesivas operaciones. Por ejemplo:

```TypeScript
let texto1 = "Buen";
let texto2 = "día";
let texto3 = "a todos";
let saludo = texto1.concat(" ", texto2).concat(" ", texto3);
console.log(saludo);

// El resultado en la consola será:
// Buen día a todos
```

## Ver también: 

- [Documentación oficial de TypeScript sobre concatenación de cadenas](https://www.typescriptlang.org/docs/handbook/strings.html#string-concatenation)
- [Tutorial en español sobre concatenación de cadenas en TypeScript](https://desarrolloweb.com/articulos/como-unir-cadenas-typescript.html)
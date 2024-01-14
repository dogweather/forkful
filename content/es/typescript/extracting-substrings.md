---
title:                "TypeScript: Extrayendo subcadenas"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas?

La extracción de subcadenas es una tarea común en la programación, especialmente cuando se trabaja con cadenas de texto. Al extraer una subcadena, podemos obtener una parte específica de una cadena más grande, lo que nos permite manipular y trabajar con ella de manera más eficiente.

## Cómo hacerlo

En TypeScript, podemos extraer una subcadena de una cadena utilizando el método `substring()` de la clase `String`. Este método toma dos parámetros: el índice de inicio y el índice de fin de la subcadena que queremos extraer.

Veamos un ejemplo de cómo extraer una subcadena en TypeScript:

```TypeScript
// Creamos una cadena de texto
let texto = "Hola a todos";

// Extraemos la subcadena a partir del tercer carácter hasta el final
let subcadena = texto.substring(2);

// Imprimimos la subcadena
console.log(subcadena);
// Salida: "la a todos"
```

También podemos especificar el índice de fin de la subcadena en el segundo parámetro del método `substring()` para extraer una parte específica de la cadena:

```TypeScript
// Creamos una cadena de texto
let texto = "Hola a todos";

// Extraemos la subcadena desde el segundo carácter hasta el cuarto
let subcadena = texto.substring(1, 4);

// Imprimimos la subcadena
console.log(subcadena);
// Salida: "ola"
```

## Profundizando

El método `substring()` en TypeScript funciona de manera similar a su equivalente en JavaScript. Sin embargo, hay un par de cosas que debemos tener en cuenta:
- El índice de inicio debe ser menor que el índice de fin. De lo contrario, se devolverá una cadena vacía.
- Si no se especifica un índice de fin, se extraerá la subcadena desde el índice de inicio hasta el final de la cadena.

Además, también podemos utilizar el método `substr()` para extraer una subcadena en TypeScript. A diferencia de `substring()`, `substr()` toma dos parámetros: el índice de inicio y la longitud de la subcadena.

## Ver también

- [Documentación de TypeScript sobre el método substring()](https://www.typescriptlang.org/docs/handbook/strings.html#substring)
- [Documentación de TypeScript sobre el método substr()](https://www.typescriptlang.org/docs/handbook/strings.html#substr)
---
title:                "TypeScript: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por qué extraer subcadenas en TypeScript

Extracción de subcadenas es una operación común en la programación que consiste en obtener una parte de una cadena de texto. En TypeScript, esta tarea se puede realizar fácilmente utilizando algunos métodos y funciones integrados en el lenguaje. En este artículo, exploraremos por qué uno podría querer extraer subcadenas y cómo hacerlo en TypeScript. También nos adentraremos en algunos conceptos más profundos y proporcionaremos ejemplos de código para ilustrar mejor el proceso.

## Cómo hacerlo

Para extraer subcadenas en TypeScript, podemos utilizar el método `substring()` en las cadenas de texto. Este método toma dos argumentos: el índice inicial de la subcadena y el índice final (opcional). Si omitimos el segundo argumento, se asume que queremos extraer todos los caracteres hasta el final de la cadena.

```TypeScript
let str: string = "Hola, mundo!";
console.log(str.substring(0, 4));

// Output: Hola
```

También podemos utilizar el método `slice()` que tiene la misma funcionalidad que `substring()`. Sin embargo, el segundo argumento de `slice()` representa la longitud de la subcadena en lugar del índice final.

```TypeScript
let str: string = "Hola, mundo!";
console.log(str.slice(5, 10));

// Output: mundo
```

También podemos extraer subcadenas utilizando el operador de indexación `[]` en las cadenas de texto. Este operador toma un índice (o rango de índices) y devuelve la subcadena correspondiente.

```TypeScript
let str: string = "Hola, mundo!";
console.log(str[6]);

// Output: m

console.log(str[2, 8]);

// Output: la, mu
```

## Profundizando en la extracción de subcadenas

Además de los métodos y operadores mencionados anteriormente, también podemos utilizar la función `substring()` en TypeScript. A diferencia del método `substring()` que se aplica a cadenas individuales, la función `substring()` se puede utilizar en una gran variedad de tipos, incluyendo cadenas, matrices y tuplas. Esto nos proporciona una flexibilidad adicional en cuanto a cómo extraemos subcadenas en nuestros programas.

También podemos utilizar expresiones regulares para extraer subcadenas en TypeScript. Las expresiones regulares nos permiten buscar patrones en una cadena de texto y extraer subcadenas basadas en esos patrones. Podemos utilizar el método `match()` junto con una expresión regular para realizar la extracción.

## Ver también

- [Documentación de TypeScript sobre extracción de subcadenas](https://www.typescriptlang.org/docs/handbook/strings.html#substring)
- [Expresiones regulares en TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expression-based-string-pattern-matching.html)
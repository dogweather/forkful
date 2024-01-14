---
title:    "TypeScript: Encontrando la longitud de una cadena"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué

A veces, cuando estamos programando en TypeScript, necesitamos saber la longitud de una cadena de texto. Ya sea para realizar validaciones, manipular datos o cualquier otra tarea, saber la longitud de una cadena es un conocimiento fundamental que nos ahorrará tiempo y esfuerzo. Afortunadamente, en TypeScript existe una forma sencilla de encontrar la longitud de una cadena de texto.

## Cómo hacerlo

Para encontrar la longitud de una cadena en TypeScript, utilizamos el método `length` en una variable que contenga la cadena. Veamos un ejemplo:

```TypeScript
let nombre: string = "Juan";
console.log(nombre.length);
```

El código anterior imprimirá `4` en la consola, ya que "Juan" tiene 4 letras. Podemos aplicar este método a cualquier cadena de texto, incluso a variables que contengan cadenas.

```TypeScript
let ciudad: string = "Madrid";
console.log(ciudad.length);
```

Este ejemplo imprimirá `6`, ya que "Madrid" tiene 6 letras. Ahora bien, ¿qué pasa si queremos conocer la longitud de una cadena que tiene caracteres especiales como acentos? En TypeScript, las letras acentuadas también cuentan como un carácter, por lo que la longitud seguirá siendo correcta.

```TypeScript
let apellido: string = "Gómez";
console.log(apellido.length);
```

El resultado de este ejemplo seguirá siendo `5`, ya que TypeScript cuenta el carácter "ó" como un solo carácter, a pesar de estar compuesto por dos caracteres. Esto lo hace una herramienta muy útil a la hora de trabajar con diferentes idiomas que utilicen caracteres acentuados.

## Profundizando

El método `length` en TypeScript es posible gracias a la propiedad `length` que tienen las cadenas de texto en JavaScript. Esta propiedad devuelve la longitud de la cadena como un número entero y se puede utilizar en cualquier cadena de texto, no solo en TypeScript.

Ahora bien, algo a tener en cuenta es que el método `length` no cuenta únicamente las letras, sino también los espacios en blanco y los símbolos de puntuación. Por lo tanto, es importante tener esto en cuenta al utilizar este método en un programa.

## Ver también

- [Documentación oficial de TypeScript sobre el método length](https://www.typescriptlang.org/docs/handbook/strings.html#string-length)
- [Tutorial de Codecademy sobre el método length en JavaScript](https://www.codecademy.com/courses/learn-javascript/lessons/strings-and-string-methods/exercises/string-length)
- [Ejemplos prácticos del método length](https://www.freecodecamp.org/news/javascript-string-length-how-to-find-the-length-of-a-string-in-js/)
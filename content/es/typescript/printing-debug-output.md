---
title:                "Imprimiendo la salida de depuración"
html_title:           "TypeScript: Imprimiendo la salida de depuración"
simple_title:         "Imprimiendo la salida de depuración"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
Muchas veces, al desarrollar una aplicación en TypeScript, nos encontramos con errores difíciles de detectar. Para solucionar estos problemas, imprimir información de debug en la consola puede ser de gran ayuda. Esta técnica nos permite inspeccionar valores y seguir el flujo de nuestro código, lo que facilita la identificación y resolución de errores.

## Cómo
Para imprimir mensajes de debug en TypeScript, podemos usar el método `console.log()`, que muestra la información en la consola del navegador o la terminal. Veamos un ejemplo:

```TypeScript
let num1: number = 5;
let num2: number = 10;
let result: number = num1 + num2;
console.log('El resultado de la suma es: ' + result);
```
El output de este código será:
```TypeScript
El resultado de la suma es: 15
```
También podemos imprimir variables directamente en `console.log()` sin necesidad de concatenar strings:

```TypeScript
let name: string = 'Juan';
console.log(name);
```
El output será:
```TypeScript
Juan
```
Otra opción útil es usar la función `JSON.stringify()` para imprimir un objeto en la consola:

```TypeScript
let person = { name: 'Ana', age: 25 };
console.log(JSON.stringify(person));
```
El output será:
```TypeScript
{"name":"Ana","age":25}
```
Incluso podemos usar la sintaxis de template strings para imprimir valores de manera más legible:

```TypeScript
let num: number = 7;
console.log(`El número es: ${num}`);
```
El output será:
```TypeScript
El número es: 7
```

## Deep Dive
Existen diferentes maneras de imprimir mensajes de debug en TypeScript, como utilizar el método `console.info()` para imprimir mensajes de información o `console.warn()` para imprimir mensajes de advertencia. También podemos usar `console.assert()` para validar una expresión y mostrar un mensaje en caso de que la expresión sea falsa.

Otra herramienta útil es el método `console.table()`, que nos permite imprimir una tabla con los valores de un array o un objeto. Veamos un ejemplo:

```TypeScript
let fruits: string[] = ['manzana', 'banana', 'naranja'];
console.table(fruits);
```
El output será:
```TypeScript
┌─────────┬─────────┐
│ (index) │ Values  │
├─────────┼─────────┤
│    0    │ 'manzana' │
│    1    │ 'banana' │
│    2    │ 'naranja' │
└─────────┴─────────┘
```
Para más información sobre las diferentes opciones que ofrece `console`, puedes consultar la [documentación oficial](https://developer.mozilla.org/en-US/docs/Web/API/Console).

## Ver también
- [Documentación oficial de TypeScript](https://www.typescriptlang.org/docs/)
- [Guía práctica de TypeScript para principiantes](https://dev.to/codechips/guide-to-typescript-for-beginners-340d)
- [Cómo usar TypeScript con React](https://www.freecodecamp.org/news/how-to-use-typescript-with-react/)
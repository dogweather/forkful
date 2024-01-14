---
title:                "TypeScript: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en TypeScript

Escribir pruebas en TypeScript es una práctica importante en el desarrollo de software que asegura que nuestro código funciona correctamente. Al realizar pruebas, podemos encontrar y corregir errores antes de que afecten a nuestro código en producción. Esto nos ahorra tiempo y esfuerzo a largo plazo y mejora la calidad de nuestro código.

## Cómo escribir pruebas en TypeScript

Para escribir pruebas en TypeScript, utilizaremos un framework de pruebas llamado Jest. Jest es una herramienta popular y fácil de usar para escribir pruebas en JavaScript y TypeScript. A continuación, se muestra un ejemplo de cómo escribir una prueba simple en TypeScript utilizando Jest:

```TypeScript
import { suma } from './calculadora';

test('suma 2 + 2 debería ser igual a 4', () => {
  expect(suma(2, 2)).toBe(4);
});
```

En este ejemplo, primero importamos la función `suma` desde nuestro archivo de código `calculadora.ts`. Luego, escribimos una prueba utilizando el método `test` de Jest. Dentro de este test, utilizamos la función `expect` para comprobar si el resultado de la suma de 2 + 2 es igual a 4 utilizando el método `toBe`. Si el resultado no es igual a 4, la prueba fallará.

## Profundizando en la escritura de pruebas

Para escribir pruebas efectivas en TypeScript, es importante conocer las diferentes herramientas y técnicas disponibles. Por ejemplo, Jest nos permite utilizar el método `describe` para agrupar nuestras pruebas en bloques lógicos y organizarlas de forma más ordenada. También podemos utilizar el método `beforeEach` para ejecutar ciertas acciones antes de cada prueba, como por ejemplo, reiniciar una aplicación o base de datos.

Otra técnica importante en la escritura de pruebas es el uso de mocks y stubs. Estas herramientas nos permiten crear objetos ficticios para simular ciertas funcionalidades y probar nuestro código en un entorno controlado. Esto es especialmente útil cuando se trabaja con dependencias externas como bases de datos o APIs.

## Ver también

- [Documentación de Jest](https://jestjs.io/docs/en/getting-started)
- [Introducción a la escritura de pruebas en TypeScript con Jest](https://www.digitalocean.com/community/tutorials/how-to-write-unit-tests-in-typescript-with-jest)
- [Curso de Jest en TypeScript](https://egghead.io/browse/languages/typescript?q=Jest)
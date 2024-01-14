---
title:                "TypeScript: Escribiendo pruebas"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en TypeScript?

Escribir pruebas en TypeScript es una manera efectiva de garantizar que nuestro código funcione correctamente y prevenir errores antes de que lleguen a producción. Además, el uso de pruebas automatizadas puede mejorar la eficiencia y velocidad en el proceso de desarrollo.

## Cómo escribir pruebas en TypeScript

Para escribir pruebas en TypeScript, primero necesitamos tener una unidad de código que queremos probar. En el siguiente ejemplo, vamos a escribir una prueba para una función que suma dos números:

```Typescript
function sum(a: number, b: number): number {
    return a + b;
}
```

Ahora, vamos a crear una prueba utilizando la librería de pruebas Jest:

```Typescript
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

El código anterior crea una prueba que verifica si la función "sum" retorna correctamente la suma de dos números. El "test" toma dos argumentos: una descripción de la prueba y una función que contiene las expectativas. En este caso, esperamos que la suma de 1 y 2 sea igual a 3, y eso es lo que la función "expect" verifica. 

## Profundizando en la escritura de pruebas

Escribir pruebas en TypeScript implica el uso de conceptos como mocks, spies y assertions. Estas herramientas nos permiten probar una unidad de código de manera aislada y verificar su comportamiento sin afectar a otros componentes del sistema.

Además, existen diferentes tipos de pruebas como las unitarias, de integración y de aceptación, cada una con su propio propósito y forma de implementación. También es importante tener en cuenta las buenas prácticas de escritura de pruebas, como tener una cobertura adecuada y nombres descriptivos para las pruebas.

## Ver también

- [Jest documentation](https://jestjs.io/docs/en/getting-started)
- [TypeScript testing with Jest](https://basarat.gitbooks.io/typescript/docs/testing/jest.html)
- [Unit testing in TypeScript](https://medium.com/@Armno/unit-testing-typescript-classes-24d98e8784bd)

_____

## ¡Por qué escribir pruebas en TypeScript es importante!

Escrito por [Tu Nombre]

Escribir pruebas en TypeScript es esencial para asegurar que nuestro código sea confiable y esté libre de errores. Además, el proceso de escribir pruebas mejora nuestro entendimiento del código y nos ayuda a escribir un código más estructurado y mantenible. ¡Empieza a escribir tus pruebas hoy mismo y verás la diferencia en tu proceso de desarrollo!
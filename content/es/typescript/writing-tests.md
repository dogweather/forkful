---
title:                "Escribiendo pruebas."
html_title:           "TypeScript: Escribiendo pruebas."
simple_title:         "Escribiendo pruebas."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas en programación es una técnica utilizada para verificar que nuestro código cumpla con los requisitos y funcione correctamente. Los programadores utilizan pruebas para asegurarse de que sus programas son robustos y libres de errores, lo que a su vez ayuda a mejorar la calidad del software.

## Cómo hacerlo:

Para escribir pruebas en TypeScript, podemos utilizar el framework de pruebas Jest. Jest nos permite escribir pruebas de forma sencilla y eficiente.

```
TypeScript import { sumar } from "./utilidades";

describe("Función de suma", () => {
  test("Debería sumar correctamente dos números", () => {
    expect(sumar(2, 3)).toBe(5);
  });
});

El resultado esperado sería "true", ya que la suma de 2 y 3 es igual a 5.
```

## Profundizando:

Escribir pruebas es una práctica común en la programación moderna, ya que nos ayuda a evitar errores y a mantener un código más limpio y organizado. Además de Jest, existen otros frameworks de pruebas como Mocha o Jasmine que también son populares entre los desarrolladores.

Otra alternativa a escribir pruebas es utilizar la técnica de TDD (Test Driven Development), donde primero se escriben las pruebas y luego se escribe el código para que cumpla con ellas. Esta técnica asegura que nuestro código sea más preciso y tenga una mayor cobertura de pruebas.

En cuanto a la implementación, es importante seguir buenas prácticas al escribir pruebas, como tener una buena estructura y nombrar las pruebas de forma descriptiva para facilitar su mantenimiento y entender el propósito de cada una.

## Véase también:

- [Jest](https://jestjs.io/)
- [Mocha](https://mochajs.org/)
- [Jasmine](https://jasmine.github.io/)
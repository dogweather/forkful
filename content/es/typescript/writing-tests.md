---
title:    "TypeScript: Escribiendo pruebas"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en TypeScript?

Escribir pruebas en TypeScript es una práctica común en el desarrollo de software ya que permite detectar errores y fallos en el código de manera temprana. También ayuda a mantener un código más limpio y organizado, lo cual facilita su mantenimiento y futuras modificaciones. Al escribir pruebas, se asegura que el código funcione correctamente y se reduce el riesgo de posibles errores en producción.

## Cómo escribir pruebas en TypeScript

Para escribir pruebas en TypeScript, se puede utilizar una herramienta de pruebas como Jest. En este ejemplo, se mostrará cómo escribir una prueba sencilla para una función que suma dos números:

```TypeScript
function sum(a: number, b: number): number {
  return a + b;
}

it('should return the sum of two numbers', () => {
  expect(sum(2, 2)).toBe(4);
});
```

Al utilizar `expect` y `toBe`, se verifica que el resultado de la función `sum` sea igual a 4. Si fuera diferente, la prueba fallaría y se señalaría un posible error.

Otra forma de escribir pruebas en TypeScript es utilizando una librería de aserciones como Chai, que ofrece más métodos para realizar diferentes tipos de comprobaciones en el código.

## Profundizando en la escritura de pruebas

Para escribir pruebas efectivas, es importante considerar diferentes casos de uso y situaciones en las que el código puede fallar. Una buena práctica es escribir pruebas para cada función o componente del código, asegurando que todas las áreas estén cubiertas. Además, se pueden utilizar herramientas como Istanbul para medir la cobertura de las pruebas y garantizar que se esté probando el código de manera adecuada.

También es importante recordar que las pruebas deben ser independientes y no deben depender de otras pruebas para funcionar correctamente. De esta manera, se asegura que cada prueba se ejecute de manera aislada y se puedan detectar errores específicos.

## Vea también

- [Jest](https://jestjs.io/)
- [Chai](https://www.chaijs.com/)
- [Istanbul](https://istanbul.js.org/)
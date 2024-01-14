---
title:                "Javascript: Escribiendo pruebas"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Escribir pruebas es una parte fundamental en el proceso de programación. No solo ayuda a detectar errores y garantizar que el código funcione correctamente, sino que también facilita la identificación de problemas y la realización de cambios en el futuro. En resumen, escribir pruebas es una forma eficaz de mejorar la calidad y la estabilidad de nuestro código.

## How To
A continuación, presentamos un ejemplo sencillo de cómo escribir y ejecutar pruebas en Javascript utilizando la biblioteca Jest. Primero, crearemos una función para sumar dos números:

```Javascript
const sum = (a, b) => {
  return a + b;
}

console.log(sum(2, 3)); // Output: 5
```

Una vez que hemos escrito nuestro código, es importante asegurarse de que funcione correctamente. Para ello, utilizaremos Jest para escribir una prueba que evalúe si nuestra función de suma retorna el resultado esperado:

```Javascript
const sum = require('./sum');

test('adds 2 + 3 to equal 5', () => {
  expect(sum(2, 3)).toBe(5);
});
```

En este ejemplo, estamos importando la función de suma y utilizando la función `test` de Jest para definir la prueba. Dentro de la función `expect`, establecemos el valor que esperamos obtener y en la función `toBe` pasamos el resultado de nuestra función de suma. Al ejecutar esta prueba, Jest nos mostrará si el resultado coincide con lo que esperamos.

## Deep Dive
Ahora que hemos visto un ejemplo sencillo de cómo escribir pruebas en Javascript, es importante profundizar en algunas buenas prácticas y conceptos clave a tener en cuenta al escribir pruebas.

Primero, es esencial tener una buena cobertura de pruebas, es decir, que todas las líneas y posibles caminos de nuestro código estén cubiertos por pruebas. Además, es importante tener una combinación de pruebas unitarias, que se centran en probar unidades individuales de código, y pruebas de integración, que son más amplias y prueban cómo funcionan juntas distintas partes de nuestro código.

Además, es importante nombrar adecuadamente nuestras pruebas para que sean claras e identificables, y utilizar aserciones (como `expect`) para verificar los resultados y no confiar en la simple impresión en la consola.

En general, escribir pruebas nos ayuda a detectar errores temprano en el proceso de desarrollo y nos da la confianza de que nuestro código funciona como esperamos.

## See Also
Si quieres seguir aprendiendo sobre escribir pruebas en Javascript, aquí tienes algunos recursos adicionales:

- [Documentación oficial de Jest](https://jestjs.io/docs/en/getting-started)
- [Curso gratuito de Aprende a Programar Ya sobre pruebas unitarias en Javascript](https://aprendeaprogramarya.com/courses/curso-de-javascript/lectures/24687412)
- [Artículo de Medium: 7 consejos para escribir pruebas en Javascript como un pro](https://medium.com/@nicknauert/7-tips-for-unit-testing-javascript-like-a-pro-2fc87fa11e08)
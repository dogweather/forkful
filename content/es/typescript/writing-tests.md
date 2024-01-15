---
title:                "Redacción de pruebas"
html_title:           "TypeScript: Redacción de pruebas"
simple_title:         "Redacción de pruebas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

¿Por qué deberías preocuparte por escribir pruebas en tu código de TypeScript? La respuesta es simple: las pruebas son una forma eficaz de garantizar que tu código funcione correctamente y se comporte de la manera que deseas. También sirven como documentación para futuras actualizaciones y cambios en el código.

## Cómo

Escribir pruebas en TypeScript es sencillo y puede darte una gran tranquilidad al desarrollar tu aplicación. Primero, asegúrate de tener una herramienta de pruebas instalada, como Jest o Mocha. Luego, sigue estos pasos:

1. Crea un archivo de prueba con la extensión `.test.ts`.
2. Importa las librerías que necesites para realizar tus pruebas.
3. Define una función de prueba usando `describe` y `it`.
4. Dentro de la función de prueba, escribe el código que deseas probar y usa `expect` para comprobar si devuelve los resultados esperados.
5. Ejecuta tus pruebas con el comando `npm test` o `yarn test`.

Aquí hay un ejemplo sencillo de una prueba de una función de suma en TypeScript:

```TypeScript
import { sumar } from './utils';

describe("sumar", () => {
  it("debería devolver la suma de dos números", () => {
    const resultado = sumar(2, 3);
    expect(resultado).toBe(5);
  })
})
```

Este código crea una prueba que espera que la función `sumar` regrese el resultado correcto al sumar 2 y 3. Si ejecutamos esta prueba y obtenemos verde, significa que nuestra función está funcionando como se espera. Si obtenemos rojo, significa que algo no está funcionando correctamente y debemos investigar y corregir el código.

## Deep Dive

Si quieres profundizar más en las pruebas en TypeScript, puedes explorar conceptos como pruebas unitarias, pruebas de integración y pruebas de aceptación. También puedes aprender cómo simular entradas de usuario y cómo probar funciones asíncronas. Recursos como la documentación oficial de Jest y tutoriales en línea pueden ayudarte a mejorar tus habilidades de escritura de pruebas.

## Ver también

- [Documentación oficial de Jest](https://jestjs.io/)
- [Tutorial de pruebas unitarias en TypeScript](https://khalilstemmler.com/articles/software-design-architecture/unit-testing-typescript/)
- [Tutorial de pruebas de integración en TypeScript con NestJS](https://blog.logrocket.com/nestjs-integration-testing/)
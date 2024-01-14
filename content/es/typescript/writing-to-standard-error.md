---
title:                "TypeScript: Escribiendo al error estándar"
simple_title:         "Escribiendo al error estándar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a estándar error en TypeScript

Escribir a estándar error es una habilidad importante en cualquier lenguaje de programación, ya que nos permite mostrar información de errores y depuración durante la ejecución de nuestro código. En TypeScript, esta práctica es especialmente útil debido a su naturaleza tipada y orientada a objetos.

## Cómo hacerlo

Para escribir a estándar error en TypeScript, podemos utilizar la función `console.error()` seguida del mensaje que deseamos imprimir. Por ejemplo, si queremos mostrar un mensaje de error en nuestra consola, podríamos escribir lo siguiente:

```TypeScript
console.error("¡Ups! Algo salió mal.")
```

Al ejecutar este código, veremos en nuestra consola el siguiente resultado:

![Ejemplo de mensaje de error en consola](https://i.imgur.com/aAKKVX1.png)

También podemos incluir variables en nuestro mensaje de error para tener una mayor comprensión de la situación. Por ejemplo:

```TypeScript
const numeroDeDocumento: number = 12345;
console.error(`El número de documento ${numeroDeDocumento} no es válido.`);
```

Al ejecutar este código, obtendremos el siguiente resultado:

![Ejemplo de mensaje de error con variable en consola](https://i.imgur.com/kTEvZIx.png)

## Un vistazo más profundo

Además de simplemente mostrar mensajes de error en nuestra consola, también podemos utilizar la función `console.error()` para mostrar información detallada sobre el estado de nuestro código. Por ejemplo, podemos imprimir el contenido de una variable para comprobar su valor en un determinado momento:

```TypeScript
const numeros: number[] = [1, 2, 3, 4];
console.error(`El valor de la variable 'numeros' es: ${numeros}`);
```

Este código resultará en el siguiente resultado en consola:

![Ejemplo de impresión de contenido de variable en consola](https://i.imgur.com/CtXZweQ.png)

Ver la información de depuración en nuestra consola puede ser extremadamente útil en la fase de desarrollo de nuestro proyecto, ya que nos permite examinar el valor de nuestras variables y validar si nuestro código está funcionando correctamente.

## Ver también

- [Documentación oficial de TypeScript sobre `console.error()`](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-3.html)
- [Artículo del blog: "Debugging en TypeScript: estrategias y mejores prácticas"] (https://blog.logrocket.com/debugging-typescript-strategies-and-best-practices/)
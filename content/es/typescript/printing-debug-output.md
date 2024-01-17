---
title:                "Imprimiendo el resultado de depuración"
html_title:           "TypeScript: Imprimiendo el resultado de depuración"
simple_title:         "Imprimiendo el resultado de depuración"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Imprimir la salida de depuración es una práctica común en la programación en la que los desarrolladores incorporan mensajes en su código para ayudar a entender y solucionar problemas durante la ejecución. Esto puede ser especialmente útil en lenguajes como TypeScript, que es un lenguaje altamente tipado y puede ser más difícil identificar errores durante la compilación.

## ¿Cómo hacerlo?

Para imprimir la salida de depuración en TypeScript, se puede usar la función console.log(). Esta función toma una o más expresiones como parámetros y las imprime en la consola del navegador o del entorno de desarrollo que se esté utilizando. Por ejemplo:

```TypeScript
let num1: number = 5;
let num2: number = 10;
console.log("El resultado de la suma es:", num1 + num2);
```

Esto imprimirá en la consola la siguiente línea:

```
El resultado de la suma es: 15
```

También se puede usar la función console.error() para imprimir mensajes de error y la función console.warn() para imprimir mensajes de advertencia.

## Profundizando

La impresión de la salida de depuración es una técnica ampliamente utilizada por los programadores desde los primeros días de la programación. Sin embargo, con la llegada de herramientas más sofisticadas de depuración, como los puntos de interrupción y los depuradores, puede parecer obsoleta. Sin embargo, todavía es una herramienta útil para la programación de TypeScript ya que puede proporcionar información oportuna durante la ejecución del código.

En lugar de imprimir mensajes en la consola, también se puede utilizar una herramienta de registro como la popular librería "debug" de Node.js. Esta librería permite un mayor control sobre los mensajes de depuración y puede ser útil para proyectos más grandes.

La impresión de la salida de depuración en TypeScript se logra mediante el uso de la API Console de JavaScript, que se encuentra en todos los navegadores modernos. Esto significa que las funciones console.log(), console.error() y console.warn() están disponibles en cualquier aplicación de TypeScript en el navegador.

## Ver también

- Documentación de TypeScript sobre la función console.log (https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#console-log-is-now-a-generic-function)
- Documentación de Node.js sobre la librería "debug" (https://github.com/visionmedia/debug)
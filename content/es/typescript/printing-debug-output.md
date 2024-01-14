---
title:    "TypeScript: Imprimiendo salida de depuración"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir la salida de depuración en TypeScript?

La impresión de la salida de depuración es una práctica común en el desarrollo de software. Al imprimir mensajes de depuración, podemos ver el estado de nuestro código en diferentes puntos de ejecución, lo que nos ayuda a identificar y solucionar errores más fácilmente.

## Cómo hacerlo

En TypeScript, podemos imprimir la salida de depuración utilizando la función `console.log()`. Vamos a ver un ejemplo sencillo:

```TypeScript
let nombre = "María";
console.log("El nombre es: " + nombre);
```

Al ejecutar este código, veremos en la consola el siguiente resultado:

```
El nombre es: María
```

Podemos imprimir cualquier tipo de dato en la salida de depuración, ya sean cadenas de texto, números, booleanos o incluso objetos y arreglos. También podemos combinar varias variables en un solo mensaje de depuración:

```TypeScript
let edad = 27;
console.log("La edad de " + nombre + " es " + edad);
```

```
La edad de María es 27
```

## Profundizando en la impresión de salida de depuración

Además de utilizar `console.log()`, TypeScript proporciona otras funciones útiles para la impresión de la salida de depuración, como `console.info()`, `console.warn()` y `console.error()`, que nos permiten imprimir diferentes tipos de mensajes de forma legible. También podemos utilizar la función `console.table()` para imprimir datos en forma de tabla.

Otra técnica útil para imprimir la salida de depuración es utilizar el operador de propagación (`...`) para imprimir todos los elementos de un arreglo o los pares clave-valor de un objeto:

```TypeScript
let numeros = [1, 2, 3, 4, 5];
console.log(...numeros);
```

```
1 2 3 4 5
```

```TypeScript
let persona = { nombre: "Juan", edad: 30 };
console.log(...persona);
```

```
Juan 30
```

También podemos utilizar condicionales y ciclos para imprimir mensajes de depuración en determinados escenarios o en múltiples puntos de nuestro código.

## Ver también
- [Documentación oficial de TypeScript sobre la impresión de salida de depuración](https://www.typescriptlang.org/docs/handbook/decorators.html)
- [Tutorial de depuración en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-debug-node-js-with-the-built-in-debugger-and-chrome-devtools)
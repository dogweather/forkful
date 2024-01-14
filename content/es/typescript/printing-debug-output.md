---
title:                "TypeScript: Imprimiendo salida de depuración"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

La impresión de salida de depuración es una herramienta valiosa para los programadores de TypeScript. Te permite ver qué sucede en tu código en tiempo real y te ayuda a identificar errores y problemas. También puede ser útil para realizar un seguimiento del flujo de ejecución de tu programa y entender mejor cómo funciona.

## Cómo hacerlo

Para imprimir salida de depuración en TypeScript, puedes utilizar la función `console.log()`. Esta función toma como parámetro cualquier tipo de datos y lo imprime en la consola del navegador o del IDE que estés utilizando. Por ejemplo:

 ```TypeScript
console.log("¡Hola mundo!");
```

Esto imprimirá "¡Hola mundo!" en la consola. También puedes imprimir variables y objetos para ver su valor en un momento determinado de la ejecución del programa:

```TypeScript
let num = 5;
console.log(num);

let persona = {
  nombre: "Ana",
  edad: 25
};
console.log(persona);
```
La salida de la consola sería:

```
5

{ nombre: "Ana", edad: 25 }
```

## Profundizando

Además de la función `console.log()`, TypeScript también tiene otras opciones para imprimir salida de depuración. Por ejemplo, puedes utilizar `console.error()` para imprimir mensajes de error en rojo en la consola o `console.warn()` para imprimir advertencias en amarillo. También puedes utilizar `console.table()` para imprimir objetos en formato de tabla y `console.time()` y `console.timeEnd()` para medir el tiempo de ejecución de una sección de código.

Es importante recordar que es recomendable eliminar todas las funciones de impresión de salida de depuración antes de publicar tu código en producción, ya que pueden ralentizar el rendimiento de la aplicación.

## Ver también

- [Documentación oficial de TypeScript sobre la impresión de salida de depuración](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#void)
- [Artículo sobre la impresión de salida de depuración en TypeScript](https://blog.logrocket.com/a-quick-guide-to-debugging-typescript-like-a-pro/)
- [Video tutorial sobre la impresión de salida de depuración en TypeScript](https://www.youtube.com/watch?v=4zXbdWNRnXY)
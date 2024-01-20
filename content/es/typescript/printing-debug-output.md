---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Imprimiendo Salida de Depuración en TypeScript: Una Guía Rápida

### Qué y Por Qué?

La salida de depuración es la información que un programa proporciona mientras se está ejecutando para ayudarte a entender lo que está pasando en su interior. Los programadores usan esto para detectar y solucionar problemas, o simplemente para monitorizar el funcionamiento de la aplicación.

### Cómo Hacerlo:

En TypeScript, a menudo usamos `console.log()` para imprimir la salida de depuración. Aquí hay un ejemplo:
```TypeScript
let mensaje = "¡Hola, mundo!";
console.log(mensaje);
```
Esto imprimirá "¡Hola, mundo!" en tu consola de salida.

Mira otro ejemplo, donde imprimimos un objeto:
```TypeScript
let objeto = {nombre: "Juan", edad: 22};
console.log(objeto);
```
Esto imprimirá `{ nombre: 'Juan', edad: 22 }` en tu consola.

### Inmersión Profunda

Históricamente, la salida de depuración ha sido una herramienta esencial para los programadores. Antes de que existieran los depuradores gráficos, las impresiones en la consola eran la única forma de entender lo que estaba sucediendo en el programa.

En TypeScript, además de `console.log()`, también tienes `console.info()`, `console.warn()`, y `console.error()`. Todas ellas funcionan de manera similar, pero se utilizan en diferentes contextos según la gravedad de los mensajes.

Con respecto a la implementación, cuando compilas TypeScript a JavaScript, `console.log()` en realidad se transforma en `console.log()` en JavaScript. Por lo tanto, al final estás utilizando la funcionalidad de JavaScript para imprimir la salida de depuración.

### Ver También

Para más información y ejemplos del uso de `console.log()` y sus variantes, te recomiendo los siguientes recursos:

- [Guía de developer.mozilla.org](https://developer.mozilla.org/es/docs/Web/API/Console/log)
- [Preguntas de Stack Overflow sobre console.log()](https://stackoverflow.com/questions/tagged/console.log)
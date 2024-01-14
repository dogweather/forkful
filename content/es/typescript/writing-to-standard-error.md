---
title:                "TypeScript: Escribiendo en el error estándar"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error estándar

Si eres un programador de TypeScript, es probable que hayas escuchado sobre la salida de error estándar. Pero, ¿por qué deberías preocuparte por escribir a ella? La respuesta es simple: cuando se ejecuta un programa, a menudo puede producir errores o mensajes de advertencia. Escribir a la salida de error estándar permite mostrar estos mensajes a los usuarios o a otros programadores, lo que puede ser útil para el diagnóstico y la solución de problemas.

## Cómo hacerlo

Para escribir a la salida de error estándar en TypeScript, puedes utilizar el método `console.error`. Por ejemplo:

```TypeScript
console.error("¡Hola a todos!");
```

Este código escribirá el mensaje "¡Hola a todos!" en la salida de error estándar. También puedes pasar múltiples argumentos al método `console.error` y estos se mostrarán en la misma línea. Por ejemplo:

```TypeScript
console.error("Error:", 404, "Página no encontrada");
```

El resultado sería `Error: 404 Página no encontrada`.

## Profundizando

La diferencia entre la salida estándar y la salida de error estándar radica en su uso. La salida estándar, que se escribe utilizando el método `console.log`, se usa para mensajes de información y detalles del programa, mientras que la salida de error estándar se utiliza para mensajes de advertencia o errores. Además, la salida de error estándar generalmente se muestra en rojo, lo que la hace más visible y fácil de identificar.

También es importante tener en cuenta que, a diferencia de la salida estándar, la salida de error estándar no se ve afectada por la redirección de salida en el terminal. Esto significa que incluso si rediriges la salida estándar a un archivo o a otro lugar, los mensajes que se escriben a través de la salida de error estándar siempre se mostrarán en la terminal.

## Ver también

- [Documentación de TypeScript sobre la salida de error estándar](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#error-handling-in-the-compiler)
- [Documentación oficial de Node.js sobre la salida estándar y de error](https://nodejs.org/api/process.html#process_process_stdout)
- [Artículo sobre la diferencia entre la salida estándar y la salida de error estándar](https://linuxhandbook.com/standard-output-vs-standard-error/)

¡Con estos conocimientos, ya estás listo para escribir a la salida de error estándar en tus programas de TypeScript! Recuerda siempre revisar la documentación oficial y buscar más información si lo necesitas. ¡Happy coding!
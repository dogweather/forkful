---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Escribir en el error estándar (`stderr`) permite separar los mensajes de error de la salida principal del programa (`stdout`). Los programadores lo utilizan para depurar y reportar errores sin interferir con la salida de datos que se podría estar usando en otro lugar.

## Cómo hacerlo:
```javascript
// Escribir un mensaje de error en stderr
console.error('Esto es un mensaje de error');

// Ejemplo con una función para manejar errores
function funcionQuePuedeFallar() {
    throw new Error('Algo salió mal');
}

try {
    funcionQuePuedeFallar();
} catch (error) {
    console.error('Error capturado:', error);
}
```
Salida de muestra en la consola:
```
Esto es un mensaje de error
Error capturado: Error: Algo salió mal
```

## Análisis Profundo
Históricamente, `stderr` se utilizó en sistemas Unix para que los errores se imprimieran de manera diferente a la salida estándar. Se puede redirigir en la consola para separar la información. Node.js implementa `process.stderr` como un stream en donde `console.error()` lo utiliza bajo el capó. Una alternativa es escribir directamente al stream usando `process.stderr.write('Mensaje de error\n')`.

## Ver También
- Documentación oficial de Node.js para `console.error()`: https://nodejs.org/api/console.html#consoleerror
- Una guía completa sobre los streams en Node.js, incluyendo `stderr`: https://nodejs.org/api/stream.html
- Explicación detallada de `stdout` vs. `stderr`: https://www.jstorimer.com/blogs/workingwithcode/7766119-when-to-use-stderr-instead-of-stdout
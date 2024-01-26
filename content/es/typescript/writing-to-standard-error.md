---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir en el error estándar significa enviar mensajes de error a un flujo especial, separado de la salida normal. Programadores lo usan para reportar problemas sin interferir con la salida de datos válidos del programa.

## Cómo hacerlo:
Aquí tienes cómo escribir en el error estándar en TypeScript. Supongamos que estamos manejando un error en una función de cálculo.

```TypeScript
function calcular() {
  try {
    // Algún código que puede fallar
  } catch (error) {
    console.error('Error detectado:', error);
  }
}

calcular();
```

Si hay un error, verás algo como esto en tu consola:

```
Error detectado: ReferenceError: xyz no está definido
```

## Profundización
Originalmente en sistemas Unix, el error estándar es uno de los tres descriptores de archivo principales. A menudo se utiliza para el logging de diagnóstico o errores. Una alternativa es escribir errores a un archivo dedicado o sistema de logging. Tecnicismos dependen del entorno: Node.js usa `process.stderr.write()` para bajo nivel de escritura.

## Ver también
- [Node.js process.stderr documentation](https://nodejs.org/api/process.html#process_process_stderr)
- Documentación de TypeScript para [console.error()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-0.html)

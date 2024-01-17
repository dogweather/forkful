---
title:                "Escribiendo en el error estándar"
html_title:           "Haskell: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir en la salida de error estándar es una forma de mostrar mensajes o información de depuración en una aplicación Haskell. Los programadores pueden utilizar esto para identificar y solucionar errores en su código de manera más eficiente.

## ¿Cómo hacerlo?
En Haskell, podemos escribir en la salida de error estándar utilizando la función `hPutStrLn` del módulo `System.IO`. Por ejemplo:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "¡Hola mundo!"
```

Esto imprimirá "¡Hola mundo!" en la salida de error estándar.

## Inmersión profunda
La práctica de escribir en la salida de error estándar se remonta a los primeros días de la programación en lenguaje C, donde se utilizaba la función `fprintf` para imprimir en la salida de error. Otros lenguajes de programación también tienen formas de escribir en la salida de error estándar, como `Console.Error.WriteLine` en C# y `System.err.println` en Java.

Alternativamente, en Haskell también es posible escribir en la salida de error utilizando la función `trace` del módulo `Debug.Trace`. Sin embargo, esta función solo se recomienda para uso temporal durante el proceso de depuración.

## Ver también
- [Documentación de `System.IO`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)
- [Documentación de `Debug.Trace`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html)
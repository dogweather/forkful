---
title:                "Author: Escribiendo en el error estándar."
html_title:           "Haskell: Author: Escribiendo en el error estándar."
simple_title:         "Author: Escribiendo en el error estándar."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Si estás escribiendo un programa en Haskell, es posible que necesites imprimir algún mensaje de error en la consola. Sin embargo, imprimir a la salida estándar puede ser confuso porque se mezcla con la salida del programa. En su lugar, puedes escribir directamente a la salida de error para destacar los mensajes de error y hacer que sea más fácil de encontrar.

## Cómo hacerlo

Para escribir a la salida de error en Haskell, puedes usar la función `hPutStrLn` del módulo `System.IO`. Esta función toma dos argumentos: el primero es el manejador de la salida (en este caso, `stderr`), y el segundo es el mensaje que quieres imprimir. Aquí hay un ejemplo de código que imprimirá un mensaje de error a la consola:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "¡Esto es un mensaje de error!"
```

Al ejecutar este programa, verás que el mensaje se imprime en la consola como se esperaba.

```
$> ghc nombre_del_archivo.hs
$> ./nombre_del_archivo
¡Esto es un mensaje de error!
```

## Profundizando

Si quieres tener un mayor control sobre cómo se manejan los mensajes de error en tu programa, puedes utilizar la función `hSetBuffering` del módulo `System.IO`. Esta función toma tres argumentos: el manejador de salida, el tipo de búfer y el modo de búfer. El tipo de búfer puede ser `NoBuffering`, `LineBuffering` o `BlockBuffering`, y el modo de búfer puede ser `NoBuffering` o `LineBuffering`.

Por ejemplo, si quieres que los mensajes de error se impriman inmediatamente en lugar de esperar a que se llene el búfer, puedes usar `hSetBuffering stderr NoBuffering`. Y si quieres que cada mensaje se imprima en una nueva línea en lugar de esperar a que se complete una línea completa, puedes usar `hSetBuffering stderr LineBuffering`.

## Ver también

- [Documentación de `System.IO`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)
- [Tutorial de Haskell en español](https://www.haskell.es/tutorial)
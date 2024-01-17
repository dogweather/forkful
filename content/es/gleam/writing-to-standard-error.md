---
title:                "Escribiendo a la salida de error estándar"
html_title:           "Gleam: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir en la salida de error estándar es una forma de imprimir mensajes de error en la consola durante la ejecución de un programa. Es útil para que los programadores puedan identificar y solucionar errores en su código de manera rápida y eficiente.

## Cómo hacerlo:

A continuación se presentan algunos ejemplos de cómo imprimir en la salida de error estándar en Gleam:

```
Gleam.debug("Este es un mensaje de error")
```

```
Gleam.fatal("¡Oh no! Algo salió mal.")
```

La salida de estos ejemplos sería:

```
[E] Este es un mensaje de error
```

```
[F] ¡Oh no! Algo salió mal.
```

## Profundizando

Escribir en la salida de error estándar ha sido una práctica común entre los programadores desde los primeros días de la programación. Alternativas a esta técnica incluyen el uso de archivos de registro o depuradores, pero escribir en la salida de error es una forma más simple y directa de identificar y solucionar errores.

En Gleam, escribir en la salida de error estándar está integrado en el lenguaje, lo que significa que no se requiere ninguna biblioteca externa para utilizarlo.

## Ver también

- Documentación de Gleam sobre imprimir en la salida de error estándar: https://gleam.run/articles/guides/error.html#printing-to-standard-error
- Otra forma de imprimir mensajes de error en Gleam utilizando el módulo "io": https://gleam.run/articles/guides/io.html#standard-error-output
---
title:    "Elm: Escribir en el error estándar"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo nos encontramos con errores y problemas que deben ser resueltos. La escritura en la salida de errores estándar, o "standard error", es una herramienta útil para identificar y solucionar estos errores. Continúa leyendo para aprender cómo escribir a la salida de errores estándar en Elm.

## Cómo hacerlo

Para escribir a la salida de errores estándar en Elm, primero debemos importar el módulo "Platform". Luego, usamos la función "log" en la que pasamos un mensaje de error como argumento. Aquí hay un ejemplo de código:

```Elm
import Platform

Platform.log "¡Este es un mensaje de error!"
```

Si ejecutamos este código en un programa de Elm, veremos que el mensaje de error se imprime en la consola.

La función "log" también acepta cualquier tipo de dato como argumento, por lo que podemos escribir variables o incluso estructuras de datos a la salida de errores estándar. Aquí hay otro ejemplo:

```Elm
import Platform

miVariable = 42

Platform.log ("El valor de miVariable es: " ++ (toString miVariable))
```

Este código imprimirá "El valor de miVariable es: 42" en la salida de errores estándar.

## Profundizando

Además de la función "log", el módulo "Platform" también ofrece otras funciones para escribir a la salida de errores estándar, como "error" y "warn". Cada una de estas funciones tiene un nivel de gravedad asociado, lo que puede ser útil para identificar la severidad de los errores en nuestro código.

También es importante tener en cuenta que al escribir a la salida de errores estándar, los mensajes se imprimirán en la consola del navegador si estamos ejecutando nuestro programa en un navegador web. Sin embargo, si estamos ejecutando nuestro programa en un servidor, los mensajes se imprimirán en el archivo de registro del servidor.

## Ver también

- [Documentación oficial de Elm sobre el módulo Platform](https://package.elm-lang.org/packages/elm/core/latest/Platform)
- [Artículo sobre cómo depurar en Elm utilizando la salida de errores estándar](https://medium.com/@isnardo/debugging-elm-code-with-standard-error-c0dfb9d4752d)
- [Foro de Elm donde se discute sobre las mejores prácticas para escribir a la salida de errores estándar](https://discourse.elm-lang.org/t/logging-best-practices/4281)
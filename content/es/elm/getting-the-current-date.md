---
title:    "Elm: Obteniendo la fecha actual"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por qué obtener la fecha actual en Elm

Obtener la fecha actual puede ser una tarea importante en cualquier aplicación. Ya sea que estés creando un calendario, una tarea programada o simplemente mostrando la fecha en una página, es útil saber cómo obtener y mostrar la fecha actual en Elm.

## Cómo hacerlo

En Elm, podemos obtener la fecha actual utilizando la función `now` del módulo `Time`. Esta función devuelve un objeto `Posix` que contiene información sobre la fecha y la hora actuales.

```Elm
import Time exposing (now)

date = now
```

El resultado de esta función será algo como esto: `Posix 2134567890`. Este número representa el número de segundos desde el 1 de enero de 1970. Pero, para nuestro propósito, necesitamos convertirlo en un formato más legible.

```Elm
date = now |> Time.toMillis |> Time.millisToPosix

-- Resultado: Posix 161 887 334 912
```

Podemos utilizar la función `millisToPosix` para convertir el resultado en milisegundos, lo que nos dará una fecha con un formato más fácil de leer.

## Profundizando

Ahora que sabemos cómo obtener la fecha actual en Elm, podemos profundizar un poco más en el tema. El módulo `Time` también ofrece una función llamada `localZone` que nos permite obtener la zona horaria local del usuario.

```Elm
import Time exposing (localZone)

zone = localZone
```

El resultado de esta función será un `TimeZone` que contiene información como el nombre y la diferencia horaria con respecto a UTC.

También podemos utilizar la función `Zone.fromUtc offset` para convertir una fecha y hora en UTC a la zona horaria local del usuario.

## Ver también

- Documentación del módulo `Time` de Elm: https://package.elm-lang.org/packages/elm/time/latest
- Ejemplo de cómo obtener la fecha actual en Elm: https://ellie-app.com/c2Nvd5yZnKta1
- Cómo mostrar la fecha actual en una página web con Elm: https://medium.com/@codebyanthony/displaying-the-date-and-time-with-elm-b292fde274fa
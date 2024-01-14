---
title:                "Elm: Imprimiendo salida de depuración"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

# ¡Imprime tus errores con Elm!

Si eres un programador de Elm, es posible que hayas escuchado sobre la importancia de imprimir tu salida de depuración. Pero, ¿por qué es tan importante? ¿Cómo puedes hacerlo? ¿Y qué más necesitas saber sobre la impresión de salida de depuración en Elm? ¡Sigue leyendo para saber más!

## ¿Por qué imprimir la salida de depuración?

La impresión de salida de depuración, también conocida como "depuración por consola", es una herramienta esencial para los programadores de Elm. Te permite ver información valiosa sobre lo que está sucediendo en tu código mientras se ejecuta. Esto es especialmente útil cuando estás tratando de encontrar y solucionar errores en tu programa. Al imprimir la salida de depuración, puedes ver el valor de ciertas variables en diferentes puntos de tu código y así identificar dónde se produce el error.

## ¿Cómo imprimir la salida de depuración?

Para imprimir la salida de depuración en Elm, simplemente utiliza la función `Debug.log` seguida de una cadena de texto y el valor que quieres imprimir. Por ejemplo:

```
elm
Debug.log "MiNumero" 42
```

Esto imprimirá la cadena de texto "MiNumero" seguida del valor 42 en la consola de tu navegador cuando ejecutes tu programa. También puedes imprimir múltiples valores separándolos por comas.

## Profundizando en la impresión de salida de depuración

La función `Debug.log` no es la única manera de imprimir la salida de depuración en Elm. También puedes usar `Debug.watch` para imprimir valores en tiempo real mientras interactúas con tu programa, y `Debug.todo` para mostrar una advertencia cuando se accede a una parte de tu código que aún no has implementado.

Además, es importante tener en cuenta que la impresión de salida de depuración solo funciona en el modo de depuración de Elm. Cuando construyes tu programa para producción, todas las funciones de depuración son eliminadas automáticamente, por lo que no afectan el rendimiento de tu aplicación final.

## Ver también

- [Documentación oficial sobre depuración en Elm] (https://guide.elm-lang.org/debugging/)
- [Artículo sobre depuración en Elm de Pragmatic Studio] (https://pragmaticstudio.com/tutorials/debugging-elm)
- [Tutorial de depuración en Elm de Elm Europe] (https://www.youtube.com/watch?v=ixB5U6de-PI)

¡Ahora que sabes cómo imprimir la salida de depuración en Elm, puedes utilizar esta herramienta para mejorar tu proceso de codificación y encontrar y solucionar errores más fácilmente!
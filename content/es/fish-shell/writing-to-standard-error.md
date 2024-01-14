---
title:    "Fish Shell: Escribiendo en el error estándar"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en el shell de Fish?

Escribir en el shell de Fish puede ser una herramienta útil para los programadores que desean tener un mayor control sobre la terminal y los errores que se pueden presentar durante la ejecución de un programa. Al escribir en la salida de error estándar, se pueden detectar y solucionar más rápidamente posibles problemas en el código.

## Cómo escribir en la salida de error estándar

```Fish Shell
echo "Hola mundo" >&2
```

Este simple comando enviará el mensaje "Hola mundo" a la salida de error estándar en lugar de la salida estándar. Esto garantiza que cualquier mensaje de error será visible para el programador, lo que puede ser especialmente útil en programas más complejos que generan múltiples tipos de salida.

## Profundizando en la escritura a la salida de error estándar

Al utilizar el operador `>&2`, el mensaje se redirige directamente a la salida de error estándar. Esto puede ser útil para controlar y gestionar cómo se manejan los errores en un programa. Además, al escribir en la salida de error estándar, se asegura que el mensaje será visible incluso si la salida estándar está siendo redirigida o silenciada.

Otra técnica útil es utilizar el comando `exit`, que permite salir del programa con un código de error específico. Por ejemplo:

```Fish Shell
if [ ! -f archivo.txt ]; then
    echo "¡Error: archivo.txt no existe!" >&2
    exit 1
fi
```

En este ejemplo, el mensaje de error se envía a la salida de error estándar y el programa se detiene con un código de error 1. Esto permite identificar específicamente el tipo de error que ocurrió y tomar las medidas adecuadas para solucionarlo.

## Ver también

- Documentación oficial de Fish Shell: https://fishshell.com/docs/current/index.html
- Guía rápida de Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Preguntas frecuentes sobre Fish Shell: https://fishshell.com/docs/current/faq.html
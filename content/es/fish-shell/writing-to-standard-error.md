---
title:                "Escribiendo a la salida de error estándar"
html_title:           "Fish Shell: Escribiendo a la salida de error estándar"
simple_title:         "Escribiendo a la salida de error estándar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en la salida de error?

Si estás escribiendo un script en Fish Shell, es importante que tengas control sobre dónde se imprime la información mientras se ejecuta el código. Escribir en la salida de error (stderr) te da una forma de separar los mensajes de error de los mensajes normales, lo que puede hacer que sea más fácil debuguear y entender lo que está pasando en tu código.

## Cómo hacerlo

Para escribir en la salida de error en Fish Shell, puedes utilizar el comando `echo` y redirigir la salida a la "salida de error estándar" (2>&1). Esto significa que cualquier texto que sea imprimido por el comando `echo` será dirigido a la salida de error en vez de a la salida estándar.

```
Fish Shell

$ echo "Este es un mensaje de error" 2>&1
Este es un mensaje de error
```

También puedes utilizar la sintaxis de redireccionamiento `|&` para redirigir la salida de un comando directamente a la salida de error. Esto puede ser muy útil cuando quieres que todos los mensajes de un comando en particular sean enviados a la salida de error.

```
Fish Shell

$ ls archivo_inexistente |& echo "Ocurrió un error al ejecutar el comando"
Ocurrió un error al ejecutar el comando
```

## Inmersión Profunda

Hay varias razones por las cuales puede ser útil escribir en la salida de error en lugar de en la salida estándar. Por ejemplo, si tienes un script que se ejecuta de forma automática en un servidor, puede ser muy importante que cualquier mensaje de error sea enviado a la salida de error para que puedas monitorearlo y solucionarlo si es necesario.

También es una buena práctica separar los mensajes de error de los mensajes normales, ya que esto te ayuda a identificar y solucionar problemas en tu código más fácilmente. Además, al escribir en la salida de error, puedes controlar cuándo y cómo se imprimen estos mensajes, lo que puede ser muy útil en situaciones específicas en las que no quieres que aparezcan errores en la salida estándar.

## Ver también

Si quieres aprender más sobre el uso de la salida de error en Fish Shell, puedes consultar los siguientes enlaces:

- [Documentación oficial de Fish Shell sobre redirección](https://fishshell.com/docs/current/tutorial.html#redirecting)
- [Tutorial de Bash sobre la salida de error](https://www.howtogeek.com/435903/what-is-stderr-standard-error-in-linux/)
- [Artículo sobre cómo escribir shell scripts en Fish Shell](https://dev.to/patpohler/writing-shell-scripts-with-fish-shell-3af2)

¡Diviértete escribiendo en la salida de error en tus scripts de Fish Shell! 🐟
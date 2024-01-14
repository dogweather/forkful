---
title:                "Fish Shell: Escribiendo en el error estándar"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Por qué

Escribir a la salida estándar de error es una habilidad útil para programadores de Fish Shell. Permite identificar errores en el código y mejorar la depuración de scripts y programas.

##Cómo hacerlo

Para escribir a la salida estándar de error en Fish Shell, se utiliza la función `echo`. Por ejemplo:

```
Fish Shell: echo "Este es un error" >&2
```

En el código anterior, el texto "Este es un error" se enviará a la salida estándar de error (stderr) en lugar de la salida estándar (stdout).

##Deep Dive

Además de la función `echo`, también es posible utilizar el operador `2>` para enviar mensajes a la salida estándar de error. Por ejemplo:

```
Fish Shell: comando_que_genera_error 2> archivo_de_errores.txt
```

Este comando redirigirá cualquier mensaje de error generado por el comando a un archivo llamado "archivo_de_errores.txt".

Otra técnica útil es utilizar el operador `|&` para redirigir tanto la salida estándar como la salida estándar de error a un mismo archivo. Por ejemplo:

```
Fish Shell: comando_a_ejecutar |& archivo_de_salida.txt
```

Este comando enviará tanto la salida estándar como la salida estándar de error del comando a un archivo llamado "archivo_de_salida.txt".

##Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guía del usuario de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Ejemplos de uso de la función `echo` en Fish Shell](https://fishshell.com/docs/current/cmds/echo.html)
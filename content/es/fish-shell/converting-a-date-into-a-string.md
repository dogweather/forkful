---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Fish Shell: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

Con la ayuda de la función `strftime` en Fish Shell, puedes convertir una fecha en formato incremental a una cadena legible por humanos. Esto es útil para mostrar fechas en un formato más amigable para el usuario, como en una interfaz de línea de comandos o en un archivo de registro.

## Cómo hacerlo

Es sencillo convertir una fecha en una cadena en Fish Shell, solo necesitas utilizar el comando `strftime` seguido de un formato y la fecha que deseas convertir. Por ejemplo:

```Fish Shell
strftime %B%d,%Y %H:%M:%S (date)
```

Este comando nos dará una salida como esta:

```Fish Shell
Noviembre 17, 2020 09:30:00
```

Puedes cambiar el formato de la fecha según tus preferencias utilizando diferentes códigos de formato. Por ejemplo, `%B` para el nombre del mes completo, `%d` para el día del mes en formato numérico y `%Y` para el año en formato de cuatro dígitos. Puedes encontrar una lista completa de los códigos de formato disponibles [aquí](https://fishshell.com/docs/current/cmds/strftime.html).

## Profundizando

La función `strftime` utiliza el estilo de formato POSIX, que es un estándar de formato de fecha y hora utilizado por muchos sistemas operativos. Además de los códigos de formato mencionados anteriormente, también puedes utilizar otros códigos para mostrar la fecha en diferentes formatos, como días de la semana, horas en formato de 12 horas, entre otros.

Por ejemplo, si deseas mostrar la fecha y hora en formato de 12 horas con el día de la semana incluido, puedes utilizar este comando:

```Fish Shell
strftime "%A %r" (date)
```

Que te dará una salida como esta:

```Fish Shell
Martes 09:30:00 AM
```

También puedes combinar diferentes códigos de formato para obtener la salida exacta que necesitas. Por ejemplo, si solo deseas mostrar el mes y el día en formato abreviado, puedes utilizar este comando:

```Fish Shell
strftime "%b %d" (date)
```

Que te dará una salida como esta:

```Fish Shell
Nov 17
```

Recuerda que siempre puedes consultar la documentación de Fish Shell para encontrar más información sobre la función `strftime` y sus opciones de formato.

## Ver también

- [Documentación de Fish Shell sobre la función `strftime`](https://fishshell.com/docs/current/cmds/strftime.html)
- [Guía de referencia para formatos de fecha y hora POSIX](https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html)
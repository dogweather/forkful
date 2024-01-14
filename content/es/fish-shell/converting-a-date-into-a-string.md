---
title:                "Fish Shell: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena de texto?

A menudo, al trabajar con programación, necesitamos mostrar fechas en un formato específico para que sean más legibles para los usuarios. La conversión de una fecha en una cadena de texto es útil en estos casos ya que nos permite personalizar el formato de la fecha según nuestras necesidades.

## Cómo hacerlo en Fish Shell

Para convertir una fecha en una cadena de texto en Fish Shell, podemos usar el comando `date` junto con el formato deseado. Por ejemplo, si queremos mostrar la fecha actual en formato DD-MM-YYYY, podemos usar el siguiente código:

```Fish Shell
date "+%d-%m-%Y"
```

Esto nos daría una salida similar a: `05-05-2021`.

Si queremos incluir también la hora en el formato, podemos usar el comando `strftime` en lugar de `date`. Por ejemplo:

```Fish Shell
strftime "%d-%m-%Y %H:%M:%S"
```

Esto nos dará una salida similar a: `05-05-2021 12:30:45`.

Es importante tener en cuenta que Fish Shell utiliza diferentes secuencias de formato para la conversión de fechas que otras shells, por lo que es importante consultar la documentación para encontrar la secuencia de formato correcta para nuestro caso específico.

## Profundizando en la conversión de fechas en Fish Shell

La conversión de fechas en Fish Shell se basa en las funciones `strftime` y `strptime`. La función `strftime` se utiliza para convertir una fecha en una cadena de texto, mientras que `strptime` se utiliza para realizar la conversión inversa, de una cadena de texto a una fecha.

Ambas funciones utilizan secuencias de formato para determinar cómo se mostrará la fecha o cómo se debe leer la cadena. Estas secuencias se comienzan con un símbolo de porcentaje (`%`) seguido de letras que representan diferentes partes de la fecha, como el día, mes, año, hora, etc.

Por ejemplo, la secuencia `%d` se utiliza para representar el día del mes, `%m` para el mes y `%Y` para el año en formato de cuatro dígitos.

Al usar estas secuencias de formato, podemos personalizar por completo cómo se muestra la fecha en nuestro programa.

## Ver también
- [Documentación de Fish Shell sobre conversiones de fecha](https://fishshell.com/docs/current/cmds/date.html)
- [Artículo sobre formatos de fechas en Fish Shell](https://www.digitalocean.com/community/tutorials/como-leer-y-formatear-fechas-en-fish-shell-es)

¡Eso es todo por hoy! Espero que esta guía te haya sido útil al aprender cómo convertir fechas en cadenas de texto en Fish Shell. ¡Hasta la próxima!
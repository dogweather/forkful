---
title:                "Fish Shell: Convirtiendo una fecha en una cadena"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena es una tarea común en la programación. Puede ser necesario para mostrar la fecha en un formato específico o para realizar operaciones con ella. En esta entrada del blog, aprenderemos cómo hacerlo utilizando Fish Shell.

## Cómo hacerlo

Para convertir una fecha en una cadena en Fish Shell, podemos utilizar el comando `date` seguido del formato deseado. Por ejemplo, si queremos mostrar la fecha en formato día/mes/año, podemos usar `date +%d/%m/%y`. El resultado sería algo así:

```Fish Shell
date +%d/%m/%y
31/05/21
```

También podemos incluir la hora si lo deseamos, simplemente agregando el formato de hora después del formato de fecha. Por ejemplo, `date +%d/%m/%y/%H/%M/%S` mostraría la fecha y la hora en el formato especificado.

## Profundizando

Fish Shell utiliza el comando `date` pero también tiene su propia función interna para convertir una fecha en una cadena. Esta función se llama `strftime` y nos permite especificar el formato de manera más detallada. Por ejemplo, si queremos mostrar la fecha en formato día de la semana, mes, día y año, podemos usar `strftime "%A, %B %d, %Y"`. El resultado sería algo así:

```Fish Shell
strftime "%A, %B %d, %Y"
Lunes, Mayo 31, 2021
```

Podemos ver que podemos especificar el orden y el tipo de información que queremos mostrar. Podemos incluir la hora, el meridiano, el año con dos o cuatro dígitos, entre otras opciones.

## Ver también

- [Fish Shell documentation - date command](https://fishshell.com/docs/current/commands.html#date)
- [Fish Shell documentation - strftime function](https://fishshell.com/docs/current/cmds/strftime.html)
- [GNU Date documentation](https://www.gnu.org/software/tar/manual/html_node/Date-input-formats.html#Date-input-formats)
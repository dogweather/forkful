---
title:                "Fish Shell: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en Fish Shell?

Hay varias razones por las cuales podrías necesitar obtener la fecha actual en tu programa de Fish Shell. Puede ser para registrar cuando se realizó una acción en particular, para mostrar la fecha actual en una interfaz de usuario o para automatizar tareas basadas en la fecha.

## Cómo hacerlo

Para obtener la fecha actual en Fish Shell, usamos el comando `date` seguido de un formato específico. Por ejemplo, si queremos mostrar la fecha en formato "día-mes-año", escribiríamos lo siguiente en la terminal:

```
Fish Shell> date +"%d-%m-%Y"
```

Esto nos devolvería la fecha actual en el formato deseado, por ejemplo: 10-06-2021. Podemos personalizar el formato según nuestras necesidades utilizando los diferentes códigos que se encuentran en la documentación de Fish Shell para el comando `date`.

## Inmersión profunda

Además del comando `date`, existen otras opciones para obtener la fecha actual en Fish Shell. Por ejemplo, podemos usar el comando `strftime` para formatear la fecha y hora, o el comando `quirkydate` que nos permite especificar una fecha y hora específicas en el formato que queramos.

Otra opción interesante es utilizar variables de Fish Shell para almacenar la fecha actual y utilizarla en nuestro código. Por ejemplo, podemos crear una variable llamada `hoy` y almacenar la fecha utilizando el comando `date`:

```
Fish Shell> set hoy (date +"%d-%m-%Y")
```

Luego, podemos llamar a esta variable en nuestro código y mostrar la fecha actual en cualquier lugar que queramos.

## Ver también

- Documentación de Fish Shell para el comando `date`: https://fishshell.com/docs/current/cmds/date.html
- Opciones de formatos para `date`: https://fishshell.com/docs/current/cmds/date.html#formatting
- Comando `strftime`: https://fishshell.com/docs/current/cmds/strftime.html
- Comando `quirkydate`: https://fishshell.com/docs/current/cmds/quirkydate.html
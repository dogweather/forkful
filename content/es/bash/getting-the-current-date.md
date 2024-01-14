---
title:                "Bash: Obteniendo la fecha actual"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías obtener la fecha actual?

Obtener la fecha actual puede ser útil por varias razones, ya sea para mostrar la fecha en un programa, para realizar cálculos de tiempo, o simplemente para tener una referencia de cuándo se realizó una acción. En este artículo, aprenderás cómo obtener la fecha actual en Bash.

## Cómo hacerlo

Para obtener la fecha actual en Bash, puedes utilizar el comando `date`. Este comando te mostrará la fecha y hora actuales en tu sistema.

```Bash
date
```

La salida puede variar de acuerdo a tu zona horaria, pero el formato por defecto es `día semana mes día año hora:minuto:segundo zona horaria`.

Si deseas cambiar el formato de salida, puedes utilizar opciones con el comando `date`. Por ejemplo, si solo deseas mostrar la fecha en formato año-mes-día, puedes utilizar la opción `-I`.

```Bash
date -I
```

Esto mostrará la fecha en el formato requerido sin la hora o la zona horaria.

Otra opción útil es la opción `-d`, que te permite obtener la fecha y hora de una fecha específica. Puedes utilizar esta opción para obtener la fecha dentro de una semana o dentro de un mes a partir de la fecha actual.

```Bash
date -d "1 week"
```

Esto mostrará la fecha dentro de una semana a partir de hoy.

## Profundizando en el comando `date`

El comando `date` ofrece muchas más opciones que las mencionadas anteriormente. Puedes usar la opción `--help` para ver una lista completa de opciones disponibles o consultar la página de manual (`man date`).

Una opción importante a tener en cuenta es la opción `-u`, que mostrará la fecha y hora en formato UTC (Coordinated Universal Time). Esto es útil si necesitas realizar cálculos de tiempo sin tener en cuenta la zona horaria.

Además, también puedes cambiar el formato de salida utilizando la opción `+`. Por ejemplo, `+%A` te mostrará el día de la semana completo (Monday, Tuesday, etc.), mientras que `+%Y` te mostrará solo el año.

## Ver También
- [5 comandos Bash básicos que debes conocer](https://ejemplos.awebdeveloper.com/5-comandos-bash-basicos-que-debes-conocer/)
- [Documentación de GNU `date` command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Guía de referencia rápida para comandos Bash](https://www.linuxtrainingacademy.com/beginners-guide-bash-commands/)
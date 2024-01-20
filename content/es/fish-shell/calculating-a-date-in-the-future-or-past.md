---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Fish Shell: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Calcular una fecha en el futuro o pasado es determinar la fecha exacta que cae un número específico de días antes o después de una fecha dada. Los programadores lo hacen para manejar las operaciones basadas en el tiempo, como los cálculos de fechas de vencimiento o eventos programados.

## Cómo hacerlo:
En Fish Shell, usamos el comando `date` para el cálculo de fechas. Aquí hay un ejemplo sencillo que muestra cómo calcular la fecha 5 días en el futuro:

```Fish Shell
set -l future_date (date -I -d "+5 day")
echo $future_date
```

Esto devolverá una fecha que es exactamente 5 días a partir de la fecha actual en formato YYYY-MM-DD.

Para calcular una fecha en el pasado, solo necesita cambiar el signo "+" por "-". Por ejemplo, para obtener la fecha de hace 5 días:

```Fish Shell
set -l past_date (date -I -d "-5 day")
echo $past_date
```

La salida será una fecha exactamente 5 días antes de la fecha actual.

## Más Detalles
Históricamente, los programadores han utilizado una variedad de métodos para calcular fechas en el futuro o pasado. Antes de la invención de lenguajes de script potentes como Fish Shell, estos cálculos a menudo requerían algoritmos complicados.

En cuanto a las alternativas, otros lenguajes como Bash, Python, o JavaScript también ofrecen funciones de cálculo de fechas.

La implementación detallada de estos cálculos depende en gran medida de las bibliotecas y las funciones proporcionadas por el lenguaje que estás utilizando. En el caso de Fish Shell, el comando `date` hace gran parte del trabajo por nosotros.

## Ver También
1. Documentación oficial de Fish Shell - [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
2. Tutorial de Fish Shell de DigitalOcean - [https://www.digitalocean.com/community/tutorials/how-to-use-the-fish-shell](https://www.digitalocean.com/community/tutorials/how-to-use-the-fish-shell)
3. Manual de la fecha de GNU - [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
---
title:                "Fish Shell: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¿Por qué calcular fechas en el futuro o en el pasado?

Calcular fechas en el futuro o en el pasado puede ser útil en muchos escenarios de programación, como por ejemplo, en la creación de tareas o recordatorios, o en el cálculo de plazos para proyectos.

# Cómo hacerlo en Fish Shell

Para calcular una fecha en el futuro o en el pasado en Fish Shell, se puede utilizar el comando `date` seguido de la opción `-d` y la fecha deseada en formato mes/día/año (MM/DD/YYYY). Por ejemplo, si queremos saber la fecha de 5 días en el futuro, escribiríamos en la terminal:

```Fish Shell
date -d "+5 days"
```
El resultado sería la fecha del día de hoy más 5 días. En el mismo formato, también se pueden sumar o restar años, semanas, horas, minutos o segundos.

Si en cambio queremos calcular una fecha específica, se puede utilizar el comando `date` seguido de la opción `-d` y la fecha deseada en formato mes/día/año (MM/DD/YYYY). Por ejemplo, si queremos saber la fecha del 25 de diciembre de 2021, escribiríamos en la terminal:

```Fish Shell
date -d "12/25/2021"
```

El resultado sería la fecha del 25 de diciembre de 2021. También se pueden especificar horas, minutos y segundos si se desea una fecha más exacta.

# Profundizando en el cálculo de fechas

Fish Shell ofrece muchas opciones para calcular fechas en el futuro o en el pasado. Se pueden combinar múltiples comandos para obtener fechas específicas, como por ejemplo:

```Fish Shell
date -d "next monday +2 weeks"
```

Este comando calcularía la fecha del próximo lunes y le sumaría 2 semanas, dando como resultado la fecha correspondiente a ese día.

Además, el comando `date` permite utilizar variables como años o meses específicos, lo que facilita la creación de scripts para automatizar el cálculo de fechas.

# Ver también

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [Cálculo de fechas en Bash](https://www.howtogeek.com/513226/how-to-do-date-math-on-linux-with-the-bash-shell/)
- [Más comandos útiles para trabajar con fechas en Fish Shell](https://ostechnix.com/how-to-calculate-and-show-date-and-time-in-linux-terminal/)
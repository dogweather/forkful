---
title:                "Bash: Calculando una fecha en el futuro o pasado"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular una fecha en el futuro o pasado?

Calcular una fecha en el futuro o pasado es útil para muchos escenarios, como planificar eventos o proyectos, programar tareas o simplemente por curiosidad. En este artículo, aprenderemos cómo calcular y manipular fechas en Bash.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado, primero necesitamos utilizar el comando `date`. Este comando nos permite mostrar o configurar la fecha y hora actuales del sistema.

Por ejemplo, si queremos mostrar la fecha actual, podemos usar el siguiente comando:

```Bash
date
```

Esto nos dará una salida similar a esta:

```Bash
Mié 05 May 2021 10:30:00 CEST
```

Si queremos calcular una fecha en el futuro o pasado, necesitaremos especificar la fecha deseada utilizando el parámetro `-d` seguido de la fecha en formato `MMDDyyyy`.

Por ejemplo, si queremos calcular la fecha que será en 10 días, podemos usar el siguiente comando:

```Bash
date -d "10 days"
```

Esto nos dará una salida similar a esta:

```Bash
Sáb 15 May 2021 10:30:00 CEST
```

También podemos realizar cálculos más complejos, como restar o sumar días, meses o años a la fecha actual. A continuación, algunos ejemplos:

Calcular la fecha en 2 semanas:

```Bash
date -d "+2 weeks"
```

Calcular la fecha de hace 1 mes:

```Bash
date -d "-1 month"
```

Calcular la fecha dentro de 2 años:

```Bash
date -d "+2 years"
```

Es importante mencionar que el formato de fecha puede variar dependiendo del sistema operativo y la configuración regional.

## Profundizando

Para una mayor flexibilidad, también podemos especificar la fecha de manera más precisa utilizando el formato `MM/DD/YYYY` seguido de la hora en formato `HH:MM:SS`.

Por ejemplo, si queremos calcular la fecha que será dentro de 3 días y 2 horas, podemos usar el siguiente comando:

```Bash
date -d "3 days 2 hours"
```

Esto nos dará una salida similar a esta:

```Bash
Sáb 08 May 2021 12:30:00 CEST
```

También podemos utilizar parámetros adicionales para especificar la zona horaria o el formato de salida deseado. Para más información, podemos consultar la documentación del comando `date`.

¡Ahora que conocemos los conceptos básicos, podemos empezar a jugar y experimentar con diferentes fechas en Bash!

## Ver también

- Documentación oficial del comando `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Tutoriales adicionales sobre fechas en Bash: https://www.linuxjournal.com/content/working-date-and-time-0
- Otras formas de manipular fechas en Bash utilizando otros comandos: https://www.tecmint.com/manipulate-filenames-date-time-linux/
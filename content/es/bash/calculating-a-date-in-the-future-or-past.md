---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Bash: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Trabajando con Fechas en Bash: Cálculos Futuros y Pasados

## ¿Qué y Por Qué?

El cálculo de fechas futuras o pasadas permite manipular y jugar con el tiempo en tus scripts. Es útil para tareas de programación como la programación de eventos, procesos de backup y pruebas de software.

## Cómo se hace:

Bash utiliza el comando `date` para trabajar con las fechas. Aquí hay unos ejemplos básicos:

Calcula la fecha de mañana:

```Bash
date -d "+1 day"
```
Output esperado:
```Bash
Thu Jul 8 08:15:42 PDT 2021
```

Obtén la fecha de hace 10 días:

```Bash
date -d "-10 day"
```
Output esperado:
```Bash
Sun Jun 27 08:20:15 PDT 2021
```
## Profundizando:

Bash ha soportado operaciones de fecha utilizando el comando `date` desde sus primeras versiones. Sin embargo, este comando depende en gran medida del sistema y la versión de Bash, por lo que existen ligeras variaciones entre sistemas Unix y Linux.

Alternativas al comando `date` en Bash incluyen el uso de comandos como `strtotime` en PHP o módulos de fecha en Python, pero están fuera del alcance del lenguaje Bash.

Las manipulaciones de fecha en Bash son sencillas, pero bajo el capó usan algoritmos de tiempo complejos que tienen en cuenta las irregularidades del calendario, lo que hace que estas operaciones sean precisas.

## Ver también: 

- Página del manual de Bash `date`: [https://man7.org/linux/man-pages/man1/date.1.html](https://man7.org/linux/man-pages/man1/date.1.html)
- Tutorial de Bash para principiantes en español: [https://likegeeks.com/es/scripting-de-bash-script-tutorial/](https://likegeeks.com/es/scripting-de-bash-script-tutorial/)
- Guía detallada de manipulación de fecha y hora en Bash en inglés: [https://www.howtogeek.com/410442/how-to-work-with-date-and-time-on-the-bash-command-line/](https://www.howtogeek.com/410442/how-to-work-with-date-and-time-on-the-bash-command-line/)
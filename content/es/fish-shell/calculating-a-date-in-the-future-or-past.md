---
title:                "Fish Shell: Calculando una fecha en el futuro o pasado"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué
En ocasiones, puede ser útil poder calcular una fecha en el futuro o en el pasado dentro de un script o programa de Fish Shell. Ya sea para planificar tareas o para realizar cálculos de fechas en el código, aprender a hacerlo puede ser muy beneficioso para los usuarios de Fish Shell.

## Cómo Hacerlo
La forma más sencilla de calcular una fecha en el futuro o en el pasado es utilizando el comando `date` de Fish Shell. Este comando nos permite manipular fechas y obtener la fecha actual en diferentes formatos. A continuación, se presenta un ejemplo de cómo utilizarlo:

```Fish Shell
# Obtener la fecha de hoy en formato DD/MM/YYYY
date +%d/%m/%Y

# Obtener la fecha de hoy en formato YYYY/MM/DD
date +%Y/%m/%d

# Calcular la fecha de mañana
date -v +1d +%d/%m/%Y

# Calcular la fecha de hace una semana
date -v -1w +%d/%m/%Y
```

Con estos ejemplos, se puede ver cómo la opción `-v` nos permite especificar si queremos sumar o restar días, semanas, meses o años a la fecha actual. Además, podemos elegir el formato de salida que queramos utilizando `%` y los caracteres correspondientes a día, mes y año. Para más información sobre los formatos de fecha, se puede consultar la documentación del comando `date`.

## Deep Dive
Si bien utilizar el comando `date` es la forma más sencilla de calcular fechas en el futuro o en el pasado, existen otras opciones que permiten una mayor personalización. Por ejemplo, es posible utilizar la librería `dateutils` en Fish Shell para realizar cálculos más complejos de fechas. Esta librería ofrece varias opciones interesantes, como la posibilidad de trabajar con diferentes calendarios o de realizar comparaciones entre fechas.

Además, si se desea realizar un cálculo de fechas más personalizado, se puede utilizar la función `strtotime()` de Fish Shell. Esta función toma como parámetro una cadena de texto que representa una fecha y nos permite realizar operaciones matemáticas con ella. Por ejemplo:

```Fish Shell
# Calcular la fecha de hace un mes
strtotime "today -1 month"

# Calcular la fecha de hace un año
strtotime "today -1 year"

# Sumar 5 días a una fecha específica
strtotime "2020-09-23 +5 days"
```

## Ver también
- [Documentación del comando `date`](https://fishshell.com/docs/current/commands.html#date)
- [Documentación de la librería `dateutils` en Fish Shell](https://fishshell.com/docs/current/cmds/dateutils.html)
- [Documentación de la función `strtotime()` en Fish Shell](https://fishshell.com/docs/current/cmds/strtotime.html)
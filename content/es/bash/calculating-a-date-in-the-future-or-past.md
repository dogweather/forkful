---
title:    "Bash: Calculando una fecha en el futuro o pasado"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué
En la programación de Bash, a menudo es necesario calcular fechas en el futuro o en el pasado. Esto puede ser útil para tareas como programar tareas y automatizar procesos. En este artículo, aprenderemos cómo realizar este tipo de cálculos utilizando Bash.

## Cómo hacerlo
Para calcular una fecha en el futuro o en el pasado, primero debemos tener una fecha base y un número de días que deseamos agregar o restar. Vamos a utilizar la función `date` en Bash para llevar a cabo estos cálculos. Aquí hay un ejemplo de cómo podemos obtener la fecha de mañana en el formato "año-mes-día":

```Bash
#!/bin/bash

# Obtenemos la fecha de hoy
today=$(date +%F)

# Agregamos un día a la fecha de hoy
tomorrow=$(date -d "$today + 1 day" +%F)

echo "La fecha de mañana es: $tomorrow"
```

La salida de este código sería:

```
La fecha de mañana es: 2021-10-04
```

Además de agregar o restar un día, también podemos utilizar valores como "weeks" (semanas), "months" (meses) o "years" (años) en lugar de "day" (día). También podemos utilizar números negativos para restar días.

## Profundizando
La función `date` en Bash ofrece muchas más opciones para cálculos de fechas en el futuro o en el pasado. Por ejemplo, podemos especificar una fecha diferente a la actual como punto de partida, utilizando la opción `-d` seguida de la fecha deseada en formato "año-mes-día". También podemos mostrar la fecha en un formato diferente utilizando la opción `+%F` o `+%D`.

Otra opción interesante es la opción `-u` que se utiliza para mostrar la fecha y hora en UTC (Tiempo Universal Coordinado). Esto es útil si necesitamos trabajar con fechas en diferentes zonas horarias.

Para conocer todas las opciones disponibles para la función `date`, podemos utilizar el comando `man date` en la terminal para acceder al manual de esta función.

## Ver también
- ["Cómo utilizar la función `date` en Bash"](https://www.ejemplos.com/manuales/date-en-bash.shtml)
- ["Cómo automatizar tareas en Bash usando la función `cron` y `crontab`"](https://www.ejemplos.com/manuales/automatizar-tareas-en-bash-crontab.shtml)
- ["Trabajando con fechas en Bash"](https://www.linuxito.com/programacion/342-trabajando-con-fechas-en-bash) (en español)
---
title:                "Bash: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado puede ser útil por varias razones, como por ejemplo, planificar eventos o citas, realizar cálculos financieros o simplemente para saber qué día caerá una fecha específica.

## Cómo hacerlo

Existen algunas formas de calcular fechas en el futuro o en el pasado en Bash. A continuación, se muestra un ejemplo de código usando el comando `date`:

```Bash
# Calcular la fecha de hoy
fecha_actual=$(date +%Y-%m-%d)
echo $fecha_actual

# Calcular la fecha de mañana
fecha_manana=$(date +%Y-%m-%d -d "+1 day")
echo $fecha_manana

# Calcular la fecha de hace una semana
fecha_pasada=$(date +%Y-%m-%d -d "1 week ago")
echo $fecha_pasada
```

La salida de este código sería:

```
2021-07-10
2021-07-11
2021-07-03
```

También se pueden realizar cálculos con fechas específicas utilizando el formato `YYYY-MM-DD`:

```Bash
# Calcular la fecha de 2 meses después de hoy
fecha_futura=$(date +%Y-%m-%d -d "+2 months")
echo $fecha_futura

# Calcular la fecha de 6 meses atrás desde hoy
fecha_pasada=$(date +%Y-%m-%d -d "6 months ago")
echo $fecha_pasada

# Calcular la fecha de 1 año atrás desde una fecha específica
fecha_anterior=$(date +%Y-%m-%d -d "2020-01-01 -1 year")
echo $fecha_anterior
```

La salida de este código sería:

```
2021-09-10
2021-01-10
2019-01-01
```

También se pueden combinar diferentes unidades de tiempo, como días, semanas, meses y años, para calcular una fecha en el futuro o en el pasado.

## Profundizando

Para calcular fechas en el futuro o en el pasado, es necesario tener en cuenta algunos aspectos importantes. Por ejemplo, el comando `date` en Bash se basa en el formato especificado en la variable `LC_TIME`, por lo que puede variar dependiendo del idioma y la configuración regional de tu sistema.

Además, es importante tener en cuenta que el comando `date` solo permite calcular fechas dentro de un rango limitado, que generalmente es desde el año 1901 hasta el año 2038. Para calcular fechas fuera de este rango, se pueden utilizar otras herramientas o librerías externas en Bash.

## Ver también

- [Cómo usar el comando "date" en Bash](https://www.hostinger.es/tutoriales/usar-comando-date-en-linux/)
- [Ejemplo de cálculo de fechas en Bash](https://www.tecmint.com/calculate-future-past-dates-in-linux/)
- [Documentación oficial del comando "date" en GNU/Linux](https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html)
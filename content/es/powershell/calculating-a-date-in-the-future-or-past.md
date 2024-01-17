---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "PowerShell: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Calcular una fecha en el futuro o pasado es una tarea importante para los programadores. Esto nos permite realizar cálculos temporales precisos y realizar tareas como enviar recordatorios a los usuarios o programar eventos en una aplicación. Además, puede ser útil para determinar cuánto tiempo ha pasado desde un evento o para predecir fechas futuras en un sistema.

## ¡Aprende cómo!

Afortunadamente, calcular una fecha en el futuro o pasado en PowerShell es muy sencillo. Simplemente podemos utilizar la cmdlet `Get-Date` y especificar la cantidad de tiempo que deseamos calcular utilizando los parámetros `-Date` y `-Adjust`. Aquí tienes un ejemplo:

```PowerShell
Get-Date -Date "04/23/2021" -Adjust 7

Output: April 30, 2021 12:00:00 AM
```

En este ejemplo, hemos especificado la fecha `04/23/2021` como la fecha base y hemos utilizado el parámetro `-Adjust` para sumar 7 días. Podemos utilizar valores negativos para restar días de la fecha base. También podemos utilizar otras unidades de tiempo como `-Adjust -1d` para restar un día, o `-Adjust 3h` para sumar tres horas.

## Bucea más profundo

Esta función de PowerShell se basa en el sistema de coordenadas de tiempo Unix, que cuenta la cantidad de segundos transcurridos desde el 1 de enero de 1970. Esto significa que podemos realizar cálculos de tiempo utilizando simples operaciones matemáticas, como sumar o restar la cantidad adecuada de segundos. Por ejemplo:

```PowerShell
(Get-Date).AddSeconds(3600)

Output: April 23, 2021 1:00:00 AM
```

Esto nos da la fecha y hora exactas que serían una hora después de la fecha actual.

Si deseas realizar cálculos más complejos, puedes utilizar el módulo `TimeSpan` de PowerShell. Este módulo nos permite realizar operaciones con intervalos de tiempo y combinarlos con la fecha y hora actual para obtener resultados precisos.

## Ver también

Para obtener más información y ejemplos sobre cómo calcular fechas en PowerShell, consulta la documentación oficial de Microsoft [aquí](https://docs.microsoft.com/es-es/powershell/scripting/handy-bits-of-code/date-time-resource-pool-63c59d85?view=powershell-7.1) y este artículo de [GeeksforGeeks](https://www.geeksforgeeks.org/powershell-get-date-command/). ¡Ahora ya puedes realizar cálculos de tiempo en tus scripts de forma rápida y sencilla!
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
Calcular una fecha en el futuro o pasado implica obtener una fecha que está a una cierta cantidad de días, meses o años de la fecha actual. Los programadores hacen esto para tareas como programar recordatorios, calcular fechas de vencimiento o proyectar fechas en escenarios de planificación.

## ¿Cómo hacerlo?
Aquí veremos cómo calcular una fecha 30 días en el futuro y 30 días en el pasado con PowerShell.

```PowerShell
# Fecha en el Futuro
$fechaFutura = (Get-Date).AddDays(30)
Escribe-Salida "La fecha 30 días en el futuro es: $fechaFutura"

# Fecha en el Pasado
$fechaPasada = (Get-Date).AddDays(-30)
Escribe-Salida "La fecha 30 días en el pasado es: $fechaPasada"
```

La salida será algo similar a esto:

```PowerShell
La fecha 30 días en el futuro es: 29 November 2022 12:00:00
La fecha 30 días en el pasado es: 30 September 2022 12:00:00
```

## Análisis Profundo
En cuanto al contexto histórico, antes de la invención de los lenguajes de programación modernos como PowerShell, calcular una fecha en el futuro o pasado era un proceso manual y complicado. PowerShell ha simplificado enormemente esta tarea con funciones incorporadas como Get-Date y AddDays.

Como alternativas, en PowerShell también puedes usar AddMonths o AddYears para calcular una fecha a una cantidad de meses o años en el futuro o pasado.

En cuanto a los detalles de implementación, Get-Date obtiene la fecha y hora actual del sistema, luego AddDays, AddMonths, o AddYears agregan (o restan) la cantidad especificada de días, meses o años.

## Ver También
Para más detalle sobre las funciones de fecha y hora en PowerShell, aquí hay algunos enlaces útiles:

- Get-Date: docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/get-date
- AddDays: docs.microsoft.com/es-es/dotnet/api/system.datetime.adddays
- AddMonths: docs.microsoft.com/es-es/dotnet/api/system.datetime.addmonths
- AddYears: docs.microsoft.com/es-es/dotnet/api/system.datetime.addyears
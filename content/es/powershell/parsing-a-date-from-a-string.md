---
title:                "Analizando una fecha desde una cadena de texto"
html_title:           "PHP: Analizando una fecha desde una cadena de texto"
simple_title:         "Analizando una fecha desde una cadena de texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analizando Fechas desde Cadenas en PowerShell 

## ¿Qué y Por Qué?

Una cadena que indica una fecha y hora se analiza para regresar un objeto de fecha en un lenguaje de programación. Los programadores hacen esto para realizar cálculos, comparaciones y otras operaciones con fechas de una manera más sencilla.

## ¿Cómo Hacerlo?

Podemos utilizar el cmdlet `Get-Date` de PowerShell para analizar una fecha desde una cadena. Aquí hay un ejemplo:

```PowerShell
$cadenaFecha = '31/12/2019 23:59'
$fecha = Get-Date -Date $cadenaFecha
echo $fecha.DayOfWeek
```

El código marca la salida `Tuesday` que es el día de la semana de la fecha analizada.

## Análisis Profundo

1. **Contexto Histórico:** PowerShell fue desarrollado por Microsoft en 2006 para ayudar a los administradores de sistema a gestionar y automatizar las tareas del sistema operativo. Desde su creación, ha estado mejorando constantemente en funcionalidad y ahora ofrece una forma efectiva de analizar fechas desde cadenas.

2. **Alternativas:** Puedes usar la función `[DateTime]::ParseExact()` para analizar una fecha desde una cadena en un formato específico. Te da un control más granular sobre el análisis de las fechas.
   
3. **Detalles de Implementación:** PowerShell utiliza la función `DateTime.TryParse()` de .NET Framework internamente para analizar la fecha desde una cadena. Intentará hacer coincidir la cadena de fecha con varios formatos conocidos.

## Ver También

Aquí algunos enlaces útiles para más detalles:

1. [Documentación Oficial de PowerShell](https://docs.microsoft.com/es-es/powershell/)
2. [Cmdlet de Get-Date](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
3. [La Clase DateTime en .NET](https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0)
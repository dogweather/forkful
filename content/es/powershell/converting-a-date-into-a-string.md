---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "PowerShell: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Convertir una fecha en una cadena de texto es simplemente tomar una fecha en formato de fecha y hora y convertirla en una representación visual más legible para los humanos. Los programadores hacen esto para facilitar la lectura y el manejo de fechas en sus scripts de PowerShell.

## Cómo:
```PowerShell
# Ejemplo 1: Convertir una fecha a una cadena de texto en formato dd/mm/yyyy
Get-Date -Format "dd/mm/yyyy"
# Salida: 07/08/2021

# Ejemplo 2: Convertir una fecha a una cadena de texto en formato MMMM d, yyyy
Get-Date -Format "MMMM d, yyyy"
# Salida: agosto 7, 2021

# Ejemplo 3: Convertir una fecha a una cadena de texto en formato yyyy/MM/dd HH:mm:ss
Get-Date -Format "yyyy/MM/dd HH:mm:ss"
# Salida: 2021/08/07 19:21:37
```

## Profundizando:
Hay varias formas de convertir una fecha en una cadena de texto en PowerShell. Una forma común es utilizar el cmdlet `Get-Date` con el parámetro `-Format` para especificar el formato deseado. También se pueden utilizar expresiones y métodos de formato de .NET como `ToString()` para personalizar aún más la salida. Alternativamente, se pueden utilizar módulos de terceros como [Carbon](https://get-carbon.org/) que ofrecen cmdlets y funciones adicionales para manejar fechas y formatos en PowerShell de forma más avanzada.

## Ver También:
- [Referencia de `Get-Date` en la documentación oficial de Microsoft](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/get-date)
- [Cómo cambiar el formato de fecha y hora en PowerShell](https://www.windowscentral.com/how-change-date-and-time-formats-powershell)
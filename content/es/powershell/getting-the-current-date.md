---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:16:00.286771-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual es tan simple como preguntar ¿qué día es hoy? Es clave para scripts de tareas diarias, registros de tiempo, o cualquier automatización dependiente de la fecha.

## Cómo hacerlo:
```PowerShell
# Obtener la fecha actual
$fechaActual = Get-Date
$fechaActual

# Ejemplo de salida
domingo, 1 de enero de 2023 10:00:00
```

## Profundizando
PowerShell, desde sus inicios en 2006 como Monad, siempre ha incluido la función `Get-Date` porque lidiar con fechas y horas es esencial en automatización. Una alternativa de bajo nivel sería invocar APIs del sistema operativo, pero eso es poco práctico. `Get-Date` es poderoso pero sencillo: puede personalizarse con formatos de cadena y es consciente de la zona horaria. Bajo el capó, se basa en el tipo `[DateTime]` de .NET, proporcionando acceso a todas sus propiedades y métodos.

## Ver También
- [Get-Date documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
- [Objetos System.DateTime de .NET](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)

---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Obtener la fecha actual es una acción común en programación que nos permite conocer el día preciso en el sistema de ejecución. Los programadores lo utilizan para marcar eventos, generar informes o manejar tareas programadas.

## Cómo hacerlo:

Aquí está el fragmento de código para obtener la fecha y hora actuales en PowerShell:

```PowerShell
$fechaActual = Get-Date
echo $fechaActual
```

El resultado de ejecutar el código anterior será algo similar a:

```PowerShell
domingo, 14 de noviembre de 2021 21:05:45
```

## Análisis en profundidad

El comando `Get-Date` de PowerShell ha existido desde el lanzamiento de PowerShell. Antes de su existencia, los programas en consola de Windows tenían que depender de hacks para obtener la fecha y hora actuales.

Alternativamente, si necesitamos solo la fecha sin la hora, se puede hacer de esta manera:

```PowerShell
$fechaActual = Get-Date -Format "yyyy-MM-dd"
echo $fechaActual
```

El resultado será así:

```PowerShell
2021-11-14
```

Bajo el capó, `Get-Date` usa .NET para obtener la fecha y hora del sistema. Por lo tanto, los resultados son tan precisos y fiables como los que obtendríamos utilizando .NET directamente.

## Vea también

Para obtener más información sobre las manipulaciones de fecha en PowerShell, consulte las siguientes fuentes:

- Documentación oficial de `Get-Date`: https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2
- Un ejemplo completo de `Get-Date`: https://www.powershellbros.com/powershell-get-date-usage-examples/
- Formatear fechas en PowerShell: https://devblogs.microsoft.com/scripting/powertip-use-powershell-to-display-current-date/
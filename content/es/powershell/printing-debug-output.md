---
title:                "Imprimiendo salida de depuración"
date:                  2024-01-20T17:53:13.935415-07:00
model:                 gpt-4-1106-preview
simple_title:         "Imprimiendo salida de depuración"

category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Imprimir mensajes de depuración es mostrar info temporal en la consola para entender qué está pasando en el código. Los programadores lo hacen para rastrear errores fácilmente y ver el flujo de su programa.

## How to:
Para imprimir mensajes de depuración en PowerShell, se usa el cmdlet `Write-Debug`. Por defecto, estos mensajes no se muestran. Para activarlos, usa `$DebugPreference`.

```PowerShell
# Activa la impresión de mensajes de depuración
$DebugPreference = "Continue"

# Mensaje de depuración simple
Write-Debug "Este es un mensaje de depuración"

# Mensaje de depuración en una función
function Test-Debug {
    [CmdletBinding()]
    Param()
    Process {
        Write-Debug "Dentro de la función Test-Debug"
    }
}

# Llamada a la función con mensaje de depuración
Test-Debug
```

Al ejecutar el script anterior, verás:

```
DEBUG: Este es un mensaje de depuración
DEBUG: Dentro de la función Test-Debug
```

## Deep Dive:
Antes del `Write-Debug`, el output de depuración era más rudimentario. Se usaban técnicas como `Write-Host` o `Write-Verbose`, pero no eran tan manejables como `Write-Debug`. Los mensajes de depuración en `Write-Debug` son especiales porque puedes activarlos o desactivarlos sin cambiar el script. Alternativamente, puedes usar `Write-Verbose` para mensajes detallados o `Write-Information` para información general. En entornos de producción, se recomienda usar un sistema de logs que registre eventos importantes de forma más permanente.

## See Also:
Para más información sobre la depuración en PowerShell y las prácticas recomendadas, visita:

- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
- [about_Logging](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Logging)
- [Write-Debug documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug)

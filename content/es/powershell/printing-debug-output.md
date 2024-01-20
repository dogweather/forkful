---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La impresión de salida de debug es un proceso que los programadores utilizan para identificar y corregir errores o problemas en su código. Es la luz interna que nos ayuda a comprender lo que sucede detrás del escenario del código.

## ¿Cómo hacerlo?

Aquí te presento un ejemplo de cómo imprimir la salida de depuración en PowerShell.

```PowerShell
# Escribir algo a la consola
Write-Host '¡Hola, mundo!'

# Para añadir un objeto a la salida de depuración
$DebugObjeto = @{ nombre = 'Juan'; edad = 21 }
Write-Debug "La información de debug del objeto es: $DebugObjeto"

# Para imprimir un mensaje de debug condicionalmente
$Debug_Condicion = $true
if($Debug_Condicion){
    Write-Debug '¡Esto se imprimirá si la condición es verdadera!'
}
```

Como puedes apreciar en el último ejemplo, la línea de debug se imprime si `$Debug_Condicion = $true`.

## Inmersión Profunda

Imprimir una salida de depuración ha sido un concepto primordial para los programadores desde que se inició la programación. En las etapas iniciales, se hacía escribiendo mensajes en tarjetas perforadas.

Today, with advanced IDEs, we have better alternatives like "Print Debugging" or "Console Logging" where the developer can watch the variables, thread states, memory allocation, etc.

Un alternativa a `Write-Debug` en PowerShell es `Write-Verbose`. Este te permite obtener más detalles, en lugar de solo mensajes de depuración.

```PowerShell
# Encendiendo Verbose
$VerbosePreference = 'Continue'

# Imprimiendo una salida verbose
Write-Verbose '¡Esto es un mensaje detallado!'
```
## Ver También

Para seguir aprendiendo, te recomiendo estos enlaces:
- [Documentación oficial de PowerShell](https://docs.microsoft.com/es-es/powershell/)
- [Tutorial de Debug en PowerShell](https://docs.microsoft.com/es-es/powershell/scripting/samples/working-with-debugging?view=powershell-7.1)
- [Uso de Write-Debug en PowerShell](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/write-debug?view=powershell-7&viewFallbackFrom=powershell-7.1)
- [Uso de Write-Verbose en PowerShell](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.core/about/about_verbose?view=powershell-7.1)
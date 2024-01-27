---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir en el error estándar significa mandar mensajes de error o diagnóstico a un flujo separado que no es el de salida estándar. Los programadores lo usan para diferenciar la salida normal de los mensajes de advertencia o error, facilitando así el manejo y la depuración del programa.

## How to:
Para escribir en el error estándar en PowerShell, usamos `Write-Error`, `Write-Host -ForegroundColor Red`, o redirigimos la salida con `2>`.

```PowerShell
# Utilizar Write-Error
Write-Error "Algo salió mal"

# Utilizar Write-Host con color para simular un error
Write-Host "Algo salió mal" -ForegroundColor Red

# Escribir directamente en el flujo de error estándar
"This is a regular output, but next comes an error" > out.txt
"Este es un mensaje de error" 2> error.txt
```

Salida esperada:
```
Algo salió mal
Algo salió mal
```

Los mensajes se mostrarán en rojo en la consola, indicando un error, y se redirigirán al archivo correspondiente si se usa la redirección.

## Deep Dive:
Originalmente, conceptos como salida estándar y error estándar provienen de la programación de sistemas Unix y se han adoptado en muchos otros sistemas operativos y lenguajes de programación. En PowerShell, aparte de las funciones `Write-Error` y `Write-Host`, hay otras alternativas para el manejo de errores, como `Throw` para excepciones y `Write-Verbose` o `Write-Debug` para mensajes detallados en tareas de depuración. La implementación de la redirección en PowerShell permite destinar la salida de error (`stderr`) a otros destinos como archivos, funciones, o ignorarla completamente usando `2>$null`.

## See Also:
- [about_Redirection](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_redirection)
- [about_Throw](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_throw)
- [about_Write-Error](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error)

---
title:                "Imprimiendo salida de depuración"
html_title:           "PowerShell: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Imprimir salida de depuración es una práctica común entre los programadores para ayudar a encontrar errores y comprender cómo funciona un programa. Al agregar ciertas líneas de código que muestran el valor de variables o la ejecución de ciertas funciones, podemos obtener una mejor comprensión de lo que está sucediendo en nuestro código.

## ¿Cómo hacerlo?
Utilizar la salida de depuración en PowerShell es fácil y puede ser realizado en pocas líneas de código. Simplemente use el cmdlet "Write-Host" seguido de la variable o contenido que desee mostrar en la consola.

```PowerShell
$var = "Hola mundo"
Write-Host $var
```

Esto imprimirá "Hola mundo" en la consola y nos ayudará a comprender el valor actual de la variable $var.

## Inmersión profunda
La impresión de salida de depuración ha existido desde los primeros días de la programación. Antes del uso de herramientas de depuración modernas, imprimir la salida de ciertos valores era la única forma de encontrar errores en un programa. Aunque hoy en día existen alternativas más avanzadas, la impresión de salida de depuración sigue siendo una práctica útil en situaciones donde no se pueden utilizar herramientas de depuración.

En PowerShell, también es posible utilizar el cmdlet "Write-Debug" para imprimir mensajes de depuración. Sin embargo, estos solo se mostrarán si el modo de depuración está habilitado.

## Ver también
Si desea obtener más información sobre la impresión de salida de depuración en PowerShell, puede consultar la documentación oficial de Microsoft aquí: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-host?view=powershell-7. También puede aprender más sobre los diferentes cmdlets disponibles para imprimir salida de depuración en el sitio web de PowerShell: https://docs.microsoft.com/en-us/powershell/developer/cmdlet/writing-debugging-ps?view=powershell-7. ¡Practique y diviértase depurando su código con la impresión de salida de depuración!
---
title:                "Escritura a la salida de error estándar"
html_title:           "PowerShell: Escritura a la salida de error estándar"
simple_title:         "Escritura a la salida de error estándar"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¡Qué y por qué?

Escribir a la salida estándar (stderr) en programación puede ser útil para mostrar errores o mensajes de aviso en una aplicación. Esto le permite al usuario entender mejor lo que está sucediendo en el programa y cómo solucionar posibles problemas.

## Cómo hacerlo:
```
# Ejemplo 1: Escribir a la salida estándar
Write-Error "¡Ups! Algo salió mal."

# Ejemplo 2: Utilizar una variable para escribir a la salida estándar
$mensaje = "No se encontró ningún archivo."
Write-Error $mensaje

# Salida:
¡Ups! Algo salió mal.
No se encontró ningún archivo.
```

## Profundizando

La salida estándar (stderr) es un canal de comunicación en la línea de comandos que se utiliza para mostrar mensajes de error en lugar de los resultados esperados. En el pasado, los programadores solían escribir a la salida estándar para mostrar mensajes de error, pero esta práctica ha disminuido en favor de otras técnicas, como lanzar excepciones o utilizar registros de eventos.

Una alternativa a escribir a la salida estándar en PowerShell podría ser utilizar cmdlets de registro, como `Start-Transcript` y `Stop-Transcript`, que registran la salida de un comando en un archivo de texto. Sin embargo, escribir a la salida estándar sigue siendo una forma sencilla y directa de mostrar mensajes de error en tiempo real.

Al escribir a la salida estándar en PowerShell, se utiliza el cmdlet `Write-Error`, que acepta una cadena o variable como argumento y escribe el mensaje a la salida estándar. Es importante tener en cuenta que al utilizar este cmdlet, el proceso del script no se detendrá y la ejecución continuará. Si se desea detener la ejecución en caso de un error, se puede utilizar el cmdlet `Throw` o `Exit`.

## Consulta también
- [Documentación de Microsoft sobre el cmdlet Write-Error](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/write-error)
- [Documentación de Microsoft sobre cmdlets de registro](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.diagnostics/?view=powershell-7)
- [Artículo de Medium sobre la importancia de escribir a la salida estándar en PowerShell](https://medium.com/@haacked/why-you-need-to-write-to-standard-error-in-powershell-b613f4ef5121)
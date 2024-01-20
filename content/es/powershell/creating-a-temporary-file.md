---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creando un archivo temporal en PowerShell

## ¿Qué y Por Qué?

Un archivo temporal es un tipo de archivo que se usa para almacenar datos temporalmente durante la ejecución de un programa. Este es útil para guardar información intermedia y evitar la re-ejecución de operaciones computacionales costosas.

## ¿Cómo Hacerlo?

En PowerShell, puedes crear un archivo temporal con el cmdlet `New-TemporaryFile`. Aquí te muestro cómo:

```PowerShell
$tempFile = New-TemporaryFile
```

Este comando creará un archivo temporal en la carpeta Temp de tu sistema. Puedes escribir en el archivo utilizando el cmdlet `Add-Content`. Por ejemplo:

```PowerShell
Add-Content $tempFile.FullName "Hola, Mundo!"
Get-Content $tempFile.FullName
```

Esto escribirá "Hola, Mundo!" en el archivo temporal y luego lo leerá.

## Análisis Profundo

Los archivos temporales se han utilizado desde los primeros días de la informática. Permiten a los programas manejar grandes cantidades de datos sin ocupar memoria de la computadora de manera permanente.

En PowerShell, siempre puedes crear un archivo temporal con `New-Item` en lugar de `New-TemporaryFile`. Pero, `New-TemporaryFile` es más fácil de utilizar ya que genera automáticamente un nombre de archivo único.

El cmdlet `New-TemporaryFile` se implementa mediante el uso interno de la función `Path.GetTempFileName` de .NET.

Recuerda: siempre debes borrar los archivos temporales cuando ya no sean necesarios.

## Ver También

Puedes encontrar más detalles sobre la gestión de archivos en PowerShell en las siguientes páginas:

1. [New-TemporaryFile (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile)
2. [Add-Content (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/add-content)
3. [Get-Content (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content) 

Espero que este artículo te haya sido de ayuda para entender mejor cómo crear y gestionar archivos temporales en PowerShell. ¡Hasta la próxima!
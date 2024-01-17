---
title:                "Creando un archivo temporal"
html_title:           "PowerShell: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Crear un archivo temporal es una práctica común entre los programadores de PowerShell. Se refiere a la creación de un archivo que se utilizará temporalmente para almacenar información durante la ejecución del programa. Esto puede ser útil para guardar datos de forma temporal y asegurarse de que no se afecten los archivos originales durante el proceso de desarrollo.

## Cómo:

Aquí hay dos ejemplos de cómo crear un archivo temporal en PowerShell:

```PowerShell
# Ejemplo 1: Crear un archivo temporal utilizando el cmdlet "New-TemporaryFile"
$tempFile = New-TemporaryFile
```
El cmdlet "New-TemporaryFile" creará un archivo temporal y lo almacenará en la variable $tempFile. Este archivo se borrará automáticamente cuando se cierre PowerShell.

```PowerShell
# Ejemplo 2: Crear un archivo temporal utilizando la función "New-Item"
$tmpFile = New-Item -Path "$env:TEMP\script_temp.txt" -ItemType File
```
Este ejemplo utiliza la función "New-Item" para crear un archivo temporal en la carpeta de archivos temporales de sistema del usuario actual. Esto le permite especificar un nombre de archivo y una ubicación específica para el archivo temporal.

## Deep Dive:

La creación de archivos temporales se ha vuelto importante en la programación debido a la necesidad de proteger los archivos originales durante el proceso de desarrollo. Antes de PowerShell, la creación de archivos temporales era más compleja y requería más líneas de código. Con el tiempo, esta tarea se ha vuelto mucho más sencilla gracias a los cmdlets y funciones específicas de PowerShell.

Además de las opciones mencionadas anteriormente, también se puede crear un archivo temporal utilizando el comando "New-Item" con otras opciones, como la creación de directorios temporales o la especificación de permisos para el archivo temporal.

## Ver también:

- Documentación de Microsoft sobre el cmdlet "New-TemporaryFile": https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7.1
- Blog dedicado a PowerShell: https://devblogs.microsoft.com/scripting/creating-a-temp-file-retaining-the-file-extension-with-powershell/
- Estudio de caso de LeakWatch sobre el uso de archivos temporales: https://blog.leakwatch.org/2020/03/finding-signal-in-the-noise-using-temporary-files-to-locate-domain-takeovers/
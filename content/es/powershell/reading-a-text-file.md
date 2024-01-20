---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Leer Archivos de Texto con PowerShell

## ¿Qué y Por qué?

Leer un archivo de texto implica procesar su contenido como una secuencia de caracteres. Los programadores lo hacen para manejar y analizar datos almacenados en formato de texto.

## Cómo hacerlo:

Con PowerShell, puedes leer un archivo de texto fácilmente usando el comando `Get-Content`. Aquí se muestra un ejemplo:

```PowerShell
Get-Content C:\archivo_de_texto.txt
```

Esta línea de código le dirá a PowerShell que muestre el contenido de 'archivo_de_texto.txt'. Veamos un ejemplo de la salida que podrías obtener:

```PowerShell
Hola Mundo
Este es un archivo de texto.
```

Si quieres leer solo ciertas líneas del archivo, puedes hacerlo con la flag `-TotalCount`:

```PowerShell
Get-Content C:\archivo_de_texto.txt -TotalCount 1
```

Esto solo mostrará la primera línea de 'archivo_de_texto.txt':

```PowerShell
Hola Mundo
```

## Análisis Profundo:

Históricamente, la lectura de archivos de texto ha estado en el núcleo de la programación. Así es como los programas pueden interactuar con los datos almacenados. 

No hay una única forma de leer archivos de texto. En lugar del comando `Get-Content`, también puedes usar el método `.NET StreamReader`:

```PowerShell
$StreamReader = New-Object System.IO.StreamReader("C:\archivo_de_texto.txt")
$StreamReader.ReadLine()
$StreamReader.Close()
```

Ten en cuenta que la lectura de archivos grandes puede ser costosa en términos de memoria si usas `Get-Content`. En este caso, el método `.NET StreamReader` es una alternativa más eficiente.

## Ver También:

Aquí tienes algunos enlaces a fuentes relacionadas que pueden ser de tu interés:

Para más detalles sobre `Get-Content`: [documentación oficial de Microsoft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)

Para leer archivos grandes de manera eficiente: [artículo sobre el uso del StreamReader](https://devblogs.microsoft.com/scripting/weekend-scripter-read-large-text-files-with-powershell/)
---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Crear un archivo temporal implica generar un archivo que guardará datos de manera provisional. Los programadores hacen esto para almacenar información temporal que puede requerir en un proceso posterior.

## Cómo se hace:

En este bloque de código, veremos cómo se crea un archivo temporal con C#.

```C#

using System;
using System.IO;

class Program {
    static void Main() {
        string tempPath = Path.GetTempFileName();

        Console.WriteLine($"Se ha creado el archivo temporal {tempPath}");
    }
}

```
Cuando ejecutas este código, obtendrás un output que luce como este:

```
Se ha creado el archivo temporal C:\\Users\\Usuario\\AppData\\Local\\Temp\\tmp9B3F.tmp
```

## Inmersión profunda

Históricamente, los archivos temporales se han utilizado en sistemas de computación desde sus inicios. Estos archivos se crean normalmente en un directorio temporal, cuyo camino varía dependiendo del sistema operativo.

Existen algunas alternativas para crear archivos temporales en C#, por ejemplo, puedes utilizar la clase `Path` con el método `GetRandomFileName` para generar un nombre de archivo aleatorio, aunque debes crear y manejar el archivo tú mismo.

A nivel de implementación, el método `GetTempFileName` de la clase `Path` crea un archivo con un nombre único en el directorio temporal y luego lo cierra, dejándote libre para utilizarlo en tu aplicación.

## Ver también

- Documentación oficial sobre la clase `Path` en C#: https://docs.microsoft.com/dotnet/api/system.io.path 
- Una detallada guía sobre cómo trabajar con archivos y directorios en C# : https://docs.microsoft.com/dotnet/standard/io/how-to-use-net-name-spaces
- Consejos para la gestión de archivos temporales en C#: https://stackoverflow.com/questions/581570/how-can-i-create-a-temp-file-in-c-sharp
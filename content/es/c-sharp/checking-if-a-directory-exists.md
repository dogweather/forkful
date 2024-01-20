---
title:                "Verificando si un directorio existe"
html_title:           "C#: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Verificar si un directorio existe puede ahorrar muchas molestias de tiempo de ejecución. Esto es útil cuando su código depende de archivos en un directorio particular, o necesita crear uno si no existe.

## ¿Cómo hacerlo?
He aquí un ejemplo de cómo se puede hacer usando la Clase Directory:

```C#
using System.IO;

public void CheckDirectoryExists(string path)
{
    if (Directory.Exists(path))
    {
        Console.WriteLine("El directorio existe.");
    }
    else
    {
        Console.WriteLine("El directorio no existe.");
    }
}
```

Si ejecuta este código y su ruta es válida, verá: "El directorio existe.". Si la ruta no es válida, verá: "El directorio no existe.".

## Desglose 

La funcionalidad para verificar si los directorios existen es algo que ha estado en C# desde .NET Framework 1.0, lanzado en 2002.

Hay otras formas de hacer esto. Podría intentar abrir un archivo en la ruta y manejar la excepción si falla. Sin embargo, usar `Directory.Exists(path)` es generalmente más rápido y más limpio.

La implementación de `Directory.Exists(path)` en .NET Core es interesante. Primero intenta obtener las características del archivo usando un llamado de sistema nativo. Si eso falla, intenta abrir un handle al directorio. Si eso falla, concluye que el directorio no existe.

## Ver también

Para una visión más profunda de la Clase Directory y sus métodos, vaya a [la documentación oficial de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.io.directory?view=net-6.0).

Mire también [este hilo](http://stackoverflow.com/questions/14899422/should-i-use-directory-exists-before-try-to-access-a-directory) en StackOverflow para una discusión sobre el uso de `Directory.Exists(path)`.
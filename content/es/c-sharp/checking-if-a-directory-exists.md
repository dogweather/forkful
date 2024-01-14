---
title:                "C#: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si existe un directorio?

Comprobar si un directorio existe antes de realizar operaciones en él es una buena práctica de programación. Esto puede ayudar a prevenir errores inesperados y garantizar un flujo de trabajo más eficiente. 

## Cómo hacerlo

Para comprobar si una carpeta existe en C#, podemos utilizar la clase `Directory` del espacio de nombres `System.IO`. A continuación se muestra un ejemplo de código que verifica si un directorio llamado "Documentos" existe en la ruta especificada:

```C#
using System.IO;

string ruta = "C:/Usuarios/Usuario/MisDocumentos";

if (Directory.Exists(ruta + "/Documentos"))
{
    Console.WriteLine("El directorio existe.");
}
else
{
    Console.WriteLine("El directorio no existe.");
}
```

La salida de este código sería "El directorio existe." Si cambiamos la ruta a una que no contenga un directorio llamado "Documentos", la salida sería "El directorio no existe." 

## Profundizando

Para comprender mejor cómo funciona la comprobación de existencia de directorios en C#, es importante conocer la diferencia entre rutas relativas y absolutas. Una ruta absoluta especifica la ubicación completa de un archivo o directorio, mientras que una ruta relativa se basa en la ubicación actual.

También es importante tener en cuenta que al verificar si un directorio existe, se comprueba si existe un archivo con el mismo nombre y extensión como directorio. Por ejemplo, si tenemos un archivo llamado "Documentos.txt" en lugar de un directorio "Documentos", la comprobación de existencia devolverá verdadero.

## Ver También

- [Documentación de la clase `Directory` en C#](https://docs.microsoft.com/es-es/dotnet/api/system.io.directory?view=net-5.0)
- [Artículo sobre rutas en C#](https://www.c-sharpcorner.com/article/c-sharp-basics-of-path-handling/)
- [Guía para desarrolladores de C# en español](https://www.tutorialsteacher.com/csharp/csharp-tutorials)
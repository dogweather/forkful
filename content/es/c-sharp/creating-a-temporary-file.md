---
title:    "C#: Creando un archivo temporal"
keywords: ["C#"]
---

{{< edit_this_page >}}

¿Por qué crear un archivo temporal?

Hay muchas situaciones en las que crear un archivo temporal puede ser útil. Por ejemplo, al descargar un archivo de internet, es común que primero se descargue en un archivo temporal antes de guardarlo en su ubicación final. También puede ser útil utilizar archivos temporales para almacenar datos de forma temporal o para realizar operaciones que requieren un archivo temporal como entrada.

## Cómo hacerlo

```C#
using System;
using System.IO;

namespace EjemploArchivosTemporales
{
    class Program
    {
        static void Main(string[] args)
        {
            // Crear un archivo temporal con prefijo "temp" y extensión ".txt"
            string rutaArchivoTemp = Path.GetTempFileName();

            // Escribir contenido en el archivo temporal
            File.WriteAllText(rutaArchivoTemp, "Este es un archivo temporal.");

            // Leer el contenido del archivo temporal y guardarlo en una variable
            string contenido = File.ReadAllText(rutaArchivoTemp);

            // Imprimir el contenido en la consola
            Console.WriteLine(contenido);

            // Borrar el archivo temporal
            File.Delete(rutaArchivoTemp);
        }
    }
}
```

**Output: Este es un archivo temporal.**

## Profundizando

Cuando creamos un archivo temporal, este se guarda en una ubicación predeterminada por el sistema operativo. En Windows, esta ubicación suele ser `"C:\Users\{usuario}\AppData\Local\Temp"`. Sin embargo, podemos especificar una ubicación personalizada utilizando la clase `Path` y sus métodos `GetTempPath()` y `GetRandomFileName()`.

También es importante tener en cuenta que los archivos temporales se eliminan automáticamente después de que se cierra el programa, pero es recomendable borrarlos manualmente después de su uso para no ocupar espacio innecesario en el disco.

## Ver también

- [Documentación de Microsoft sobre archivos temporales en C#](https://docs.microsoft.com/es-es/dotnet/api/system.io.path.gettempfilename)
- [Tutorial de programación en C# - archivos y directorios](https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/file-system/)
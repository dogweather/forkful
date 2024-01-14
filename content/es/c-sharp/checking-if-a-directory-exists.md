---
title:    "C#: Comprobando si existe un directorio"
keywords: ["C#"]
---

{{< edit_this_page >}}

## ¿Por qué?

A menudo, en programación, necesitamos saber si un directorio (carpeta) existe o no antes de realizar ciertas operaciones. Esto puede ser necesario para asegurarse de que los archivos que estamos buscando están en el lugar correcto o para crear un nuevo directorio si no existe. En general, verificar la existencia de un directorio es una práctica útil en la gestión de archivos y carpetas en una aplicación.

## ¿Cómo hacerlo?

Para verificar si un directorio existe en C#, podemos utilizar el método `Directory.Exists()` de la clase `System.IO`. Este método toma como parámetro una cadena con la ruta del directorio que queremos verificar y devuelve un valor booleano que indica si el directorio existe o no.

```C#
if(Directory.Exists("C:\\Users\\NombreDeUsuario\\Documentos\\Proyecto"))
{
    Console.WriteLine("El directorio existe.");
}
else
{
    Console.WriteLine("El directorio no existe.");
}

// Output: El directorio no existe.
```

En el ejemplo anterior, estamos verificando si el directorio "Proyecto" existe en la carpeta "Documentos" de un usuario. Como el directorio no existe, se imprimirá "El directorio no existe." en la consola.

## Profundizando

Cuando usamos el método `Directory.Exists()`, es importante tener en cuenta que solo verifica la existencia del directorio especificado y no de la ruta completa. Esto significa que si tenemos una ruta como "C:\\Users\\NombreDeUsuario\\Documentos\\Proyecto\\Carpeta1", el método solo verificará si "Carpeta1" existe y no si "Proyecto" o "Documentos" existen.

Además, debemos tener en cuenta que este método solo verifica la existencia de un directorio, no de un archivo. Si queremos verificar si un archivo específico existe, debemos usar el método `File.Exists()` de la misma clase `System.IO`.

## Consulte también

- [Directory.Exists() Metodo (System.IO)](https://docs.microsoft.com/es-es/dotnet/api/system.io.directory.exists?view=netcore-3.1)
- [File.Exists() Metodo (System.IO)](https://docs.microsoft.com/es-es/dotnet/api/system.io.file.exists?view=netcore-3.1)
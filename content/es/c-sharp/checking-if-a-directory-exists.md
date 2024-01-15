---
title:                "Comprobando si existe un directorio"
html_title:           "C#: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué?

A veces, mientras se está programando en C#, es necesario comprobar si un directorio existe antes de realizar ciertas operaciones en él. Esta verificación puede evitar errores y mejorar el rendimiento del programa.

## Cómo hacerlo

Para comprobar si un directorio existe en C#, podemos utilizar el método `Directory.Exists()` de la librería `System.IO`.

```C#
if (Directory.Exists("ruta/directorio"))
{
    Console.WriteLine("El directorio existe.");
}
else
{
    Console.WriteLine("El directorio no existe.");
}
```

En este ejemplo, utilizamos un condicional `if` para verificar si el directorio especificado en la ruta existe. Si el método `Directory.Exists()` devuelve `true`, significa que el directorio existe y se muestra un mensaje en la consola. En caso contrario, se muestra otro mensaje.

## Detalles más profundos

El método `Directory.Exists()` devuelve un valor booleano que indica si el directorio existe o no. Sin embargo, no nos proporciona información adicional, como por ejemplo si se trata de un directorio válido o si tenemos permisos para acceder a él.

Para obtener más detalles sobre el directorio, podemos utilizar el método `Directory.GetDirectories()` de la misma librería. Este método devuelve una matriz con los subdirectorios del directorio especificado, lo que nos permite acceder a información como el tamaño o la fecha de creación del directorio.

```C#
string[] subdirectorios = Directory.GetDirectories("ruta/directorio");

if (subdirectorios.Length > 0)
{
    Console.WriteLine("El directorio tiene " + subdirectorios.Length + " subdirectorios.");
}
else
{
    Console.WriteLine("El directorio está vacío.");
}
```

En este ejemplo, utilizamos el método `Length` de la matriz `subdirectorios` para obtener el número de subdirectorios del directorio especificado. Si el valor es mayor que 0, significa que hay subdirectorios y se muestra un mensaje sobre esto. De lo contrario, se muestra un mensaje indicando que el directorio está vacío.

## Ver también

- [Documentación oficial de Microsoft sobre el método Directory.Exists()](https://docs.microsoft.com/es-es/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Ejemplos prácticos utilizando Directory.Exists()](https://www.codeproject.com/Tips/324971/Avoid-errors-by-checking-if-a-directory-exists-befo)
- [Tutorial en vídeo sobre cómo comprobar si un directorio existe en C#](https://www.youtube.com/watch?v=a7zW-ZS39No)
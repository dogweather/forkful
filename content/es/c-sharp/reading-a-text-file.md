---
title:                "C#: Leyendo un archivo de texto."
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en C#

Si eres un programador en C# (pronunciado "C Sharp") y necesitas manipular datos almacenados en un archivo de texto, entonces leer y entender cómo hacerlo es una habilidad importante a tener. En este artículo, aprenderás los conceptos básicos para leer un archivo de texto en C# y cómo aplicarlos en tu propio código.

## Cómo leer un archivo de texto en C#

Para leer un archivo de texto en C#, primero debemos inicializar un objeto de la clase `StreamReader` que nos permitirá acceder al archivo. Utilizando el método `ReadLine()`, podemos leer cada línea del archivo y almacenarla en una variable de tipo string. A continuación, podemos imprimir o manipular esos datos según sea necesario. A continuación se presenta un ejemplo de código para leer y mostrar el contenido de un archivo de texto utilizando la estructura `try-catch`:

```C#
StreamReader archivo = new StreamReader(@"C:\Users\Usuario\Archivo.txt");

try
{
    string linea;
    while ((linea = archivo.ReadLine()) != null)
    {
        Console.WriteLine(linea);
    }
}
catch (Exception e)
{
    Console.WriteLine("Error: " + e.Message);
}
finally
{
    archivo.Close();
}
```

La salida del código anterior mostraría todas las líneas del archivo, una después de la otra.

## Deep Dive: Información más detallada sobre la lectura de archivos de texto en C#

Además de `ReadLine()`, la clase `StreamReader` también tiene otros métodos útiles para leer archivos de texto, como `Read()`, `ReadBlock()` y `ReadToEnd()`. Cada uno de estos métodos tiene su propio propósito y características, por lo que es importante familiarizarse con ellos.

También es importante mencionar el bloque `try-catch-finally` utilizado en nuestro ejemplo. Este es un bloque de código que se utiliza para manejar posibles errores que puedan ocurrir durante la lectura del archivo. Es importante seguir buenas prácticas de programación y siempre manejar posibles errores en nuestro código.

## Ver también

- [Documentación oficial de Microsoft sobre la clase StreamReader] (https://docs.microsoft.com/es-es/dotnet/api/system.io.streamreader?view=net-5.0)
- [Ejemplos de lectura de archivos de texto en C#] (https://www.c-sharpcorner.com/blogs/read-text-file-in-c-sharp1)
- [Tutorial de C# para principiantes] (https://www.tutorialspoint.com/csharp/index.htm)
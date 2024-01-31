---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"

category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Escribir un archivo de texto significa guardar datos en un archivo en forma de texto. Los programadores lo hacen para almacenar información de manera simple, como configuraciones, logs o intercambiar datos entre programas.

## How to:
Escribir archivos es sencillo con C#. Aquí dos ejemplos usando `File.WriteAllText` y `StreamWriter`.

```C#
// Ejemplo con File.WriteAllText
string path1 = "ejemplo1.txt";
string contenido1 = "¡Hola, archivo!";
File.WriteAllText(path1, contenido1);
```

```C#
// Ejemplo con StreamWriter
string path2 = "ejemplo2.txt";
using (StreamWriter writer = new StreamWriter(path2))
{
    writer.WriteLine("Primera línea");
    writer.WriteLine("Segunda línea");
}
```

Salida (ejemplo1.txt and ejemplo2.txt):
```
¡Hola, archivo!
```
```
Primera línea
Segunda línea
```

## Deep Dive
Antes, escribir archivos era más complicado; requería manejar buffers y entender el bajo nivel de I/O del sistema. Hoy, `System.IO` simplifica todo. Las alternativas incluyen `File.AppendAllText` para añadir texto, o `File.Create` para mayor control. Es importante manejar excepciones y recursos de forma apropiada con `try-catch` y `using`, especialmente con archivos grandes o operaciones críticas.

## See Also
Para más detalles y métodos de lectura/escritura, consulta la documentación oficial de Microsoft:

- Documentación de `StreamWriter`: [https://docs.microsoft.com/dotnet/api/system.io.streamwriter](https://docs.microsoft.com/dotnet/api/system.io.streamwriter)
- Documentación de `File`: [https://docs.microsoft.com/dotnet/api/system.io.file](https://docs.microsoft.com/dotnet/api/system.io.file)
- Conceptos de manejo de archivos en general: [https://docs.microsoft.com/dotnet/standard/io](https://docs.microsoft.com/dotnet/standard/io)

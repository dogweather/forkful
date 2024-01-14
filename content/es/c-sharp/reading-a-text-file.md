---
title:    "C#: Leyendo un archivo de texto"
keywords: ["C#"]
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en C#?

Algunas veces, necesitamos acceder a la información almacenada en archivos de texto en nuestros programas en C#. Ya sea para leer una lista de nombres, importar datos de un archivo CSV o extraer información de un archivo de configuración, leer un archivo de texto es una habilidad fundamental para cualquier programador en C#.

## Cómo leer un archivo de texto en C#

Leer un archivo de texto en C# es un proceso sencillo. Primero, necesitamos definir una variable para almacenar la ruta del archivo que queremos leer. Luego, utilizamos el método `File.ReadAllLines` para leer todas las líneas del archivo y guardarlas en una matriz de strings. Finalmente, podemos recorrer esa matriz e imprimir cada línea por consola.

```C#
string rutaArchivo = "miArchivo.txt";
string[] lineas = File.ReadAllLines(rutaArchivo);

foreach (string linea in lineas)
{
    Console.WriteLine(linea);
}
```

La salida de este código sería cada línea del archivo de texto impresa en la consola. Es importante tener en cuenta que el método `File.ReadAllLines` lee todo el contenido del archivo y lo almacena en la memoria, por lo que no se recomienda para archivos muy grandes. En ese caso, podemos utilizar el método `File.ReadLines` que lee las líneas del archivo de manera "perezosa", es decir, solo carga la línea actual en la memoria cuando es necesaria.

## Profundizando en la lectura de archivos de texto en C#

Además de los métodos mencionados anteriormente, C# ofrece una gran variedad de herramientas para leer archivos de texto de manera más avanzada. Por ejemplo, podemos utilizar la clase `StreamReader` para leer archivos de manera secuencial, lo que es útil cuando necesitamos procesar grandes cantidades de datos. También podemos utilizar la clase `TextFieldParser` para leer archivos CSV con facilidad, ya que nos permite definir el delimitador y manejar campos entre comillas.

Además, es importante tener en cuenta algunos aspectos importantes al leer archivos de texto, como la codificación del archivo. Por defecto, C# utilizará la codificación UTF-8, pero es posible especificar otra codificación al utilizar los métodos `File.Read*(rutaArchivo, Encoding)`.

## Ver también

- Documentación oficial de Microsoft sobre la lectura de archivos en C#: https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file
- Tutorial de C# en español: https://www.tutorialesprogramacionya.com/csharpya/
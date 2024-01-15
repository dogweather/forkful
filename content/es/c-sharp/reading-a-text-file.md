---
title:                "Leyendo un archivo de texto"
html_title:           "C#: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Leer archivos de texto es una tarea común en la programación, ya sea para leer datos de entrada, procesar información o escribir resultados en un archivo. En este artículo, aprenderemos cómo leer un archivo de texto en C# y profundizaremos en este proceso.

## Cómo hacerlo

Primero, necesitamos usar la clase `StreamReader` para abrir y leer un archivo de texto. Esta clase se encuentra en el espacio de nombres `System.IO` y proporciona métodos para leer caracteres, cadenas y líneas de un archivo de texto. Para leer un archivo, primero debemos crear un objeto `StreamReader` y especificar la ruta al archivo que queremos leer.

```
using System.IO;

StreamReader archivo = new StreamReader("ruta/de/archivo.txt");
```

A continuación, podemos utilizar el método `ReadLine()` para leer una línea completa del archivo. Este método devuelve una cadena con el contenido de la línea leída. Podemos seguir llamando a este método para leer el archivo línea por línea.

```
string linea = archivo.ReadLine();
Console.WriteLine(linea); // Imprime la primera línea del archivo
```

Si queremos leer el archivo completo, podemos utilizar un bucle `while` para seguir leyendo líneas hasta que lleguemos al final del archivo, que se indica con un valor `null`.

```
string linea;
while ((linea = archivo.ReadLine()) != null)
{
    Console.WriteLine(linea);
}
```

Al final, es importante cerrar el objeto `StreamReader` para liberar los recursos utilizados.

```
archivo.Close();
```

## Profundizando en la lectura de archivos de texto

La clase `StreamReader` también tiene métodos para leer caracteres individuales y cadenas de un archivo de texto. Estos métodos pueden ser útiles si queremos procesar el contenido del archivo de manera más detallada.

Además, podemos especificar un tipo de codificación al crear el objeto `StreamReader`, lo que nos permite leer archivos en diferentes formatos (por ejemplo, UTF-8 o ASCII).

Por último, también podemos utilizar el método `ReadToEnd()` para leer todo el contenido del archivo de una sola vez en una cadena.

## Ver también

- [Documentación oficial de Microsoft sobre la clase StreamReader en C#](https://docs.microsoft.com/es-es/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Tutorial práctico de lectura y escritura de archivos en C#](https://www.freecodecamp.org/news/how-to-read-and-write-data-in-c-sharp/)

¡Espero que este artículo te haya sido útil para aprender a leer archivos de texto en C#! Recuerda siempre cerrar los objetos `StreamReader` después de su uso para evitar problemas de memoria. ¡Happy coding! 👨‍💻
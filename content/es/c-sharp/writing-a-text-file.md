---
title:                "Escribir un archivo de texto"
html_title:           "C#: Escribir un archivo de texto"
simple_title:         "Escribir un archivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto puede ser una tarea común en la programación en C #. Es importante saber cómo crear y manipular archivos de texto en su código para almacenar datos y facilitar la lectura y escritura de información.

## Cómo hacerlo

Para escribir un archivo de texto en C #, primero debemos importar la biblioteca "System.IO". Luego, podemos usar el objeto "StreamWriter" para crear y escribir en un archivo de texto. Aquí está un ejemplo de código:

```C#
using System.IO;

var writer = new StreamWriter("archivo.txt"); //crea un archivo llamado "archivo.txt"
writer.WriteLine("Este es un ejemplo de texto"); //escribe en el archivo de texto
writer.Close(); //cierra el archivo
```

Este código creará un archivo de texto llamado "archivo.txt" en la ubicación donde se encuentra el código. El archivo contendrá la línea de texto "Este es un ejemplo de texto".

También podemos agregar más líneas de texto al archivo antes de cerrarlo. Podemos hacer esto usando el método "WriteLine" nuevamente o el método "Write" si no queremos agregar un salto de línea. Aquí está un ejemplo de cómo agregar una nueva línea de texto al archivo existente:

```C#
var writer = new StreamWriter("archivo.txt", true); //usa "true" para indicar que queremos agregar a un archivo existente
writer.WriteLine("Esta es otra línea"); //agrega una nueva línea de texto al archivo
writer.Close();
```

Ahora, nuestro archivo de texto "archivo.txt" contendrá dos líneas de texto.

## Profundizando

Además de simplemente escribir en un archivo de texto, también podemos leer y manipular archivos de texto en C #. Podemos usar el objeto "StreamReader" para leer un archivo de texto línea por línea. Aquí hay un ejemplo de cómo imprimir las líneas de texto de un archivo:

```C#
using System.IO;

var reader = new StreamReader("archivo.txt"); //abre el archivo para lectura
string linea;

while ((linea = reader.ReadLine()) != null) { //lee línea por línea hasta llegar al final del archivo
    Console.WriteLine(linea); //imprime la línea actual
}

reader.Close(); //cierra el archivo
```

También podemos usar métodos de la clase "File" para verificar si un archivo existe, crear un nuevo archivo, borrar un archivo, etc. Tener un buen conocimiento sobre la manipulación de archivos de texto en C # puede mejorar la eficiencia de su código y ayudar a manejar la información de manera más eficaz.

## Ver también

- [Documentación de Microsoft sobre escritura en archivos de texto en C#](https://docs.microsoft.com/es-es/dotnet/standard/io/how-to-write-text-to-a-file)
- [Ejemplos de código de escritura de archivos de texto en C#](https://www.tutlane.com/tutorial/csharp/csharp-write-text-to-file)
- [Otra guía útil para escribir archivos de texto en C#](https://www.c-sharpcorner.com/blogs/working-with-text-file-in-c-sharp-in-smarter-way)
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

## ¿Qué y por qué?
¡Hola a todos! En este artículo vamos a hablar sobre cómo leer un archivo de texto en C# y, más importante aún, por qué lo hacemos. Leer un archivo de texto simplemente significa obtener los datos almacenados en un archivo de texto y mostrarlos en nuestra aplicación. Los programadores lo hacen para acceder a información importante y utilizarla en su código.

## ¡Vamos a ello!
Ahora, veamos cómo podemos leer un archivo de texto usando C#. Primero, necesitamos importar el espacio de nombres `System.IO` para poder trabajar con archivos. Luego, utilizamos la clase `StreamReader` junto con el método `ReadToEnd()` para leer todo el contenido del archivo y guardarlo en una variable. Finalmente, podemos imprimir el contenido en la consola o utilizarlo en nuestro código.

```C#
using System.IO;

namespace ReadingTextFile
{
  class Program
  {
    static void Main(string[] args)
    {
      StreamReader reader = new StreamReader("miarchivo.txt"); // Abrir el archivo de texto
      string contenido = reader.ReadToEnd(); // Leer todo el contenido
      Console.WriteLine(contenido); // Imprimir en la consola
    }
  }
}
```
¡Genial! Ahora, si el archivo de texto contiene los nombres de las ciudades del mundo, el resultado en la consola sería algo así:

```
Nueva York
Tokio
Londres
Sídney
```

## Profundizando
Leer archivos de texto en la programación ha estado presente desde los primeros días. Es una forma sencilla y eficiente de almacenar y acceder a datos. Sin embargo, también hay otras formas de hacerlo, como leer archivos binarios o utilizar bases de datos.

En cuanto a la implementación en C#, es importante recordar cerrar el archivo después de haberlo utilizado utilizando el método `Close()` o `Dispose()`. También podemos especificar el formato de lectura, como UTF-8 o ASCII, utilizando el constructor de la clase `StreamReader`.

## Véase también
Si quieres saber más sobre cómo trabajar con archivos en C#, ¡échale un vistazo a estos recursos!

- Documentación oficial de Microsoft sobre la clase `StreamReader`: https://docs.microsoft.com/es-es/dotnet/api/system.io.streamreader?view=net-5.0
- Ejemplos de código en GitHub: https://github.com/search?q=C%23+read+text+file&type=Repositories
---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

---
# Lectura de un archivo de texto en C# - Una guía fácil 

## ¿Qué y Por qué?
Lectura de un archivo de texto es el proceso de recuperar la información almacenada en un archivo en formatos legibles. Los programadores lo hacen para almacenar o recuperar datos, configuraciones, y registros útiles.

## ¿Cómo se hace?
Aquí tienes un ejemplo de cómo leer un archivo de texto línea por línea:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\miArchivo.txt";

        using (StreamReader sr = File.OpenText(path))
        {
            string s;
            while ((s = sr.ReadLine()) != null)
            {
                Console.WriteLine(s);
            }
        }
    }
}
```
Cuando corres este programa, te mostrará todo el contenido de `miArchivo.txt` en la consola.

Un ejemplo de la salida sería así:

```
Hola, mundo!
Esto es una prueba.
```

## Inmersión Profunda

**Contexto Histórico:** Los métodos para leer archivos de texto en C# han existido desde el primer versión del lenguaje. Han sido fundamentales para interactuar con la capacidad de almacenamiento de la computadora.

**Alternativas:** Existen varias maneras de leer un archivo en C#. Por ejemplo, puedes usar `File.ReadAllLines()`o `File.ReadAllText()` si deseas leer todo el archivo a la vez.

**Detalles de implementación:** Al leer un archivo de texto, debes considerar la posibilidad de excepciones de I/O. Conviértete en el hábito de usar el bloque `try-catch` para manejar estas excepciones correctamente.

## También puedes ver 

- Documentación oficial de Microsoft sobre `StreamReader`: [Link](https://docs.microsoft.com/es-es/dotnet/api/system.io.streamreader?view=net-5.0)
---
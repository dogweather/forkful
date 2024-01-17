---
title:                "Leyendo argumentos de línea de comando"
html_title:           "C#: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

Leer argumentos de línea de comandos es una técnica que permite a los programadores recibir información desde la línea de comandos al ejecutar un programa. Esto es útil para ajustar el comportamiento de un programa o proporcionar datos de entrada.

# Cómo:

Para leer argumentos de línea de comandos en C#, primero debes incluir el espacio de nombres ``` System ```, y luego utilizar la clase ``` Environment ``` y su propiedad ``` GetCommandLineArgs ```. Esta propiedad devuelve un arreglo de cadenas que contiene todos los argumentos ingresados en la línea de comandos al ejecutar el programa.

Veamos un ejemplo:

``` c#
using System;

namespace CommandLineArguments
{
    class Program
    {
        static void Main(string[] args)
        {
            // Obtener los argumentos de línea de comandos
            string[] arguments = Environment.GetCommandLineArgs();

            // Mostrar cada argumento en una línea separada
            Console.WriteLine("Argumentos de línea de comandos:");

            foreach (string argument in arguments)
            {
                Console.WriteLine(argument);
            }
        }
    }
}
```

Si compilamos y ejecutamos el programa con los argumentos ``` primero segundo tercer ```, el resultado será el siguiente:

```
Argumentos de línea de comandos:
CommandLineArguments.exe
primero
segundo
tercer
```

# Profundizando:

Esta técnica ha existido desde los primeros días de la informática, cuando los programas eran ejecutados a través de una línea de comandos. Sin embargo, aunque sigue siendo una práctica común, hoy en día existen alternativas más avanzadas, como interfaces gráficas de usuario o configuración de archivos de texto.

Para implementar la lectura de argumentos de línea de comandos en un proyecto de C#, se pueden utilizar librerías especializadas que ofrecen funciones adicionales, como manejo de errores o validación de los argumentos ingresados.

# Ver también:

- Documentación oficial de Microsoft sobre argumentos de línea de comandos en C#: https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs
- Ejemplos de código más avanzados de lectura de argumentos de línea de comandos en C#: https://www.c-sharpcorner.com/article/using-command-line-arguments-in-C-Sharp/
---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué? 
Leer los argumentos de la línea de comandos implica acceder a la información que se pasa a un programa cuando se ejecuta. Los programadores lo hacen para proporcionar parámetros adicionales sin tener que modificar el código.

## Cómo hacerlo: 

```C#
class Program
{
    static void Main(string[] args)
    {
        // Si se proporcionan argumentos, se imprimen en la consola
        if (args.Length > 0)
        {
            Console.WriteLine("Los argumentos de la línea de comandos son:");
            foreach (string arg in args)
            {
                Console.WriteLine(arg);
            }
        }
        else
        {
            Console.WriteLine("No se proporcionaron argumentos de línea de comandos.");
        }
    }
}
```

Salida de muestra:

```bash
> dotnet run arg1 arg2 arg3
Los argumentos de la línea de comandos son:
arg1
arg2
arg3
```

## Inmersión profunda 

1. Contexto histórico: El uso de argumentos de línea de comandos es una práctica que se remonta a los días en que se interactuaba principalmente con las computadoras a través de interfaces de línea de comandos.
2. Alternativas: Si los argumentos de la línea de comandos no se ajustan a tus necesidades, las entradas del usuario, la lectura de archivos, las variables de entorno o intercambiar información a través de una base de datos, también son opciones viables.
3. Detalles de implementación: los argumentos de la línea de comandos en C# se representan como una matriz de strings. La plataforma .NET las pasa al método `Main` cuando se inicia una aplicación.

## Ver también 

1. Documentación oficial de Microsoft sobre cómo usar args [] en Main: https://docs.microsoft.com/es-es/dotnet/csharp/programming-guide/main-and-command-args/
2. Publicación del blog sobre el uso de argumentos de la línea de comandos en C#: https://www.c-sharpcorner.com/article/command-line-arguments-in-c-sharp/
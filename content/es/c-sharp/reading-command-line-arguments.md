---
title:                "C#: Leyendo argumentos de línea de comando"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por Qué

En la programación en C#, puede ser necesario pasar información a través de la línea de comandos al ejecutar un programa. Leer argumentos de línea de comandos es una habilidad importante para cualquier desarrollador de C# y permite una mayor flexibilidad y personalización en el uso del programa.

## Cómo Hacerlo

Leer argumentos de línea de comandos en C# es bastante sencillo. Primero, en el método "Main" de tu código, declara un arreglo de strings llamado "args" y asigna a este el valor de "Environment.GetCommandLineArgs()". Este arreglo contendrá todos los argumentos pasados a través de la línea de comandos.

```C#
string[] args = Environment.GetCommandLineArgs();
```

A continuación, puedes recorrer el arreglo para acceder a cada argumento individualmente. Por ejemplo, supongamos que nuestro programa se llama "calculadora" y queremos pasar dos números como argumentos para realizar una operación:

```C#
double num1 = Convert.ToDouble(args[1]);
double num2 = Convert.ToDouble(args[2]);
```

Finalmente, puedes escribir el código de tu programa y probarlo pasando diferentes argumentos a través de la línea de comandos.

## Profundizando

Además de acceder a cada argumento individualmente, también puedes obtener información adicional sobre los argumentos pasados. Por ejemplo, puedes utilizar el método "Array.IndexOf()" para verificar si un argumento específico ha sido pasado o no.

```C#
if (Array.IndexOf(args, "-h") > -1)
{
    // Se ha pasado el argumento "-h", mostrar la ayuda del programa
}
```

¡Explora diferentes métodos y técnicas para leer y utilizar argumentos de línea de comandos en C# y mejora tus habilidades de programación!

## Ver También

- [Documentación oficial de C# sobre Environment.GetCommandLineArgs()](https://docs.microsoft.com/es-es/dotnet/api/system.environment.getcommandlineargs)
- [Cómo pasar argumentos de línea de comandos en C#](https://www.freecodecamp.org/news/pass-command-line-arguments-in-csharp/)
- [Usando argumentos de línea de comandos en C#](https://www.c-sharpcorner.com/article/using-command-line-arguments-in-C-Sharp/)
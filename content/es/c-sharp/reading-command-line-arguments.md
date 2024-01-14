---
title:                "C#: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos de línea de comandos

Leer argumentos de línea de comandos es una habilidad útil para cualquier programador de C#. Te permitirá crear programas que puedan recibir inputs específicos al momento de ejecutarlos, lo que puede mejorar la experiencia del usuario y hacer que tus programas sean más versátiles.

## Cómo hacerlo

Para leer argumentos de línea de comandos en C#, podemos utilizar la clase `Environment` y su método `GetCommandLineArgs()`. Por ejemplo:

```C#
//declarar una variable para almacenar los argumentos
string[] arguments;

//obtener los argumentos de la línea de comandos y asignarlos a la variable
arguments = Environment.GetCommandLineArgs();

//iterar sobre cada argumento e imprimirlo en la consola
foreach(string arg in arguments)
{
    Console.WriteLine(arg);
}
```

Si ejecutamos este código en la línea de comandos y le pasamos algunos argumentos, por ejemplo `dotnet myProgram.cs arg1 arg2`, obtendremos la siguiente salida:

```
dotnet
myProgram.cs
arg1
arg2
```

Como se puede ver, los argumentos se almacenan en un array de strings y podemos acceder a cada uno de ellos para manipularlos en nuestro programa.

## Deep Dive

Además de leer y almacenar los argumentos de la línea de comandos, también podemos realizar validaciones y convertirlos a otros tipos de datos si es necesario. Por ejemplo, podemos convertir un argumento a un número entero utilizando el método `int.Parse()`.

También es importante mencionar que los argumentos de la línea de comandos se pueden combinar con otros métodos de interacción con el usuario, como la lectura de inputs por consola o la entrada por formularios, para crear programas más complejos y dinámicos.

## Ver también

- [Microsoft Docs - Environment.GetCommandLineArgs Method](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=netcore-3.1)
- [YouTube - Reading Command Line Arguments in C#](https://www.youtube.com/watch?v=Lr_qxgA_GAg)
- [Stack Overflow - Passing Command Line Arguments to C# Program](https://stackoverflow.com/questions/10493482/passing-command-line-arguments-to-c-sharp-program)
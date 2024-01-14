---
title:    "C#: Leyendo argumentos de línea de comando"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo algunos programas reciben información directamente desde la línea de comandos? ¿Te gustaría aprender a hacerlo tú mismo en tus proyectos de C#? En este artículo te explicaré los fundamentos de cómo leer argumentos de línea de comandos en C# y cómo puedes usar esta habilidad en tus propios proyectos.

## Cómo hacerlo

La lectura de argumentos de línea de comandos en C# es una habilidad esencial para cualquier programador. Te permite enviar información al programa directamente desde la línea de comandos antes de ejecutarlo. Eso significa que puedes personalizar la forma en que se ejecuta el programa sin tener que modificar y recompilar el código.

Para leer argumentos de línea de comandos en C#, utilizamos la clase `Environment` y el método `GetCommandLineArgs()`. Este método devuelve un arreglo de cadenas que contiene todos los argumentos pasados al programa. A continuación, puedes acceder a cada argumento por separado y utilizarlo en tu código.

Veamos un ejemplo de código para leer argumentos de línea de comandos en C#:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        // Obtener los argumentos de línea de comandos
        string[] arguments = Environment.GetCommandLineArgs();

        // Imprimir todos los argumentos
        foreach(string arg in arguments)
        {
            Console.WriteLine(arg);
        }
    }
}
```

Si ejecutamos este programa en la línea de comandos y le pasamos algunos argumentos, veremos que se imprimen todos los argumentos que hemos ingresado. Por ejemplo, si ejecutamos `programa.exe argumento1 argumento2 argumento3`, la salida será:

```
programa.exe
argumento1
argumento2
argumento3
```

También puedes acceder a argumentos específicos utilizando sus índices en el arreglo. Por ejemplo, si queremos imprimir solo el tercer argumento, podemos usar `Console.WriteLine(arguments[2])` y la salida será `argumento3`.

## Profundizando

Ahora que sabemos cómo leer argumentos de línea de comandos en C#, es importante mencionar algunas consideraciones adicionales. Por ejemplo, el primer argumento en el arreglo siempre será el nombre del programa, y los índices de los argumentos comienzan en 0, es decir, el primer argumento tendrá el índice 0, el segundo el índice 1, y así sucesivamente.

Además, es común que los argumentos se pasen en forma de pares clave-valor, donde un argumento es la clave y el siguiente es el valor correspondiente. Por ejemplo, podríamos tener `programa.exe -mañana 10 -tarde 15`, donde `-mañana` y `-tarde` son las claves y `10` y `15` son los valores.

Para acceder a los valores en este formato, podemos utilizar un ciclo `for` en lugar de un `foreach`, y en cada iteración, podemos obtener la clave y el valor utilizando los índices correspondientes.

## Ver también

- [Documentación oficial de Microsoft sobre Environment.GetCommandLineArgs()](https://docs.microsoft.com/es-es/dotnet/api/system.environment.getcommandlineargs?view=net-5.0)
- [Tutorial de C# de Codecademy sobre argumentos de línea de comandos](https://www.codecademy.com/learn/learn-c-sharp/modules/learn-csharp-command-line-arguments/cheatsheet)
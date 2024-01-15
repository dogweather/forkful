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

## ¿Por qué?
Si eres un programador en C#, seguramente estás familiarizado con la idea de usar argumentos de línea de comando en tus programas. Pero si eres nuevo en el lenguaje, puede que te preguntes ¿por qué deberías utilizarlos? La respuesta es simple: los argumentos de línea de comando pueden hacer que tu código sea más dinámico y flexible, permitiéndote interactuar con el programa de diversas maneras.

## Cómo hacerlo
Usar argumentos de línea de comando en C# es muy sencillo. Simplemente necesitas acceder a los argumentos proporcionados por el usuario en la función Main(). Esto se hace utilizando el parámetro string[] args dentro de la función. Aquí hay un ejemplo que muestra cómo imprimir los argumentos ingresados por el usuario:

```C#
static void Main(string[] args)
{
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine(args[i]);
    }
}
```

Si ejecutas este programa desde la línea de comando e ingresas varios argumentos, como por ejemplo "Hola" y "Mundo", el output sería:
```
Hola
Mundo
```

Puedes utilizar esta técnica para crear programas que dependan de la entrada del usuario de manera dinámica, lo que los hace más interactivos y útiles.

## Profundizando
Ahora que sabes cómo acceder a los argumentos de línea de comando en C#, es importante que entiendas cómo funciona este proceso. Cuando ejecutas un programa desde la línea de comandos, puedes incluir argumentos después del nombre del archivo ejecutable. Este conjunto de argumentos se pasa como un array de strings al parámetro args en la función Main(). Esto significa que puedes acceder a los argumentos ingresados utilizando indexación y un ciclo for, como se mostró en el ejemplo anterior.

Es importante tener en cuenta que los argumentos de línea de comando son sensibles a mayúsculas y minúsculas. También puedes utilizar comillas dobles para incluir espacios en un solo argumento. Por ejemplo, si ingresas "Hola mundo" como argumento, se tratará como un solo elemento dentro del array, a pesar de tener un espacio en blanco.

## Ver también
Ahora que tienes una comprensión básica de cómo leer argumentos de línea de comando en C#, ¡puedes comenzar a utilizarlos en tus programas! Si quieres profundizar más en el tema, puedes revisar estos recursos adicionales:
- Documentación oficial de Microsoft sobre argumentos de línea de comando en C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments
- Un tutorial paso a paso sobre cómo usar argumentos de línea de comando en C#: https://www.c-sharpcorner.com/UploadFile/mahesh/ReadingCommandLineArgs01282006012101AM/ReadingCommandLineArgs.aspx
- Ejemplos de código de argumentos de línea de comando en C#: https://www.c-sharpcorner.com/uploadfile/mahesh/readingcommandlineargs/
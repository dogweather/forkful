---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Iniciar un nuevo proyecto en programación es el proceso de configuración inicial del ambiente y estructura de tu código. Los programadores inician nuevos proyectos para organizar y establecer una base sólida para su software.

## Cómo hacer:

Para empezar un nuevo proyecto en C#, primero abre tu terminal y técnicamente puedes usar dotnet.

```C#
dotnet new console -o MiProyecto
```

Esto crea un nuevo proyecto de consola en un directorio llamado `MiProyecto`. Si navegas a esa carpeta (`cd MiProyecto`), verás un archivo `Program.cs`, que incluye un programa de 'Hola Mundo'.

```C#
using System;

namespace MiProyecto 
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("¡Hola Mundo!");
        }
    }
}
```

Al ejecutar `dotnet run`, verás:

```C#
> dotnet run
¡Hola Mundo!
```

## Inmersión Profunda:

El comando `dotnet new` fue introducido con .NET Core, una reimplementación de .NET Framework con énfasis en la portabilidad y la velocidad. Este comando simplifica enormemente la creación de nuevos proyectos en comparación con soluciones anteriores.

Existen alternativas tradicionales para comenzar un nuevo proyecto, como la antigua técnica de copiar y pegar desde un proyecto existente o configurarlo manualmente en un editor de texto. Sin embargo, `dotnet new` es generalmente más rápido y menos propenso a errores.

Los detalles de la implementación de un nuevo proyecto varían dependiendo de las necesidades. Por ejemplo, podrías querer utilizar plantillas de proyectos (`dotnet new react`, `dotnet new mvc`, etc.) o ajustar la configuración del proyecto en el archivo `.csproj`.

## Ver También:

Para profundizar en las posibilidades de `dotnet new` y las configuraciones de proyecto de C#, consulta las siguientes fuentes:

- Guía de Microsoft sobre el comando `dotnet new`: [link](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-new?tabs=netcore21)
- Documentación de Microsoft sobre archivos `.csproj` en C#: [link](https://docs.microsoft.com/en-us/visualstudio/msbuild/msbuild?view=vs-2019)
- Guía de programación de C# de Microsoft: [link](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/)
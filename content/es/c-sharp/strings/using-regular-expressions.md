---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:11.608543-07:00
description: "Las expresiones regulares (regex) en C# son una herramienta poderosa\
  \ para la coincidencia de patrones dentro de cadenas, lo que permite a los\u2026"
lastmod: '2024-02-25T18:49:55.539188-07:00'
model: gpt-4-0125-preview
summary: "Las expresiones regulares (regex) en C# son una herramienta poderosa para\
  \ la coincidencia de patrones dentro de cadenas, lo que permite a los\u2026"
title: Usando expresiones regulares
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Las expresiones regulares (regex) en C# son una herramienta poderosa para la coincidencia de patrones dentro de cadenas, lo que permite a los programadores buscar, reemplazar, dividir o extraer datos de manera eficiente. Los programadores utilizan regex para tareas que van desde validaciones simples, como la verificación del formato de correo electrónico, hasta tareas complejas de procesamiento de texto debido a su flexibilidad y rendimiento.

## Cómo:

### Coincidencia de Patrones Simple
Para verificar si una cadena contiene un patrón específico, puedes utilizar el método `Regex.IsMatch` del espacio de nombres `System.Text.RegularExpressions`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Salida: True
    }
}
```

### Extracción de Datos
Extraer datos de una cadena usando grupos en un regex se puede hacer con el método `Regex.Match`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Fecha: 2023-04-12";
        string pattern = @"Fecha: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Año: {match.Groups[1].Value}");  // Salida: Año: 2023
            Console.WriteLine($"Mes: {match.Groups[2].Value}");  // Salida: Mes: 04
            Console.WriteLine($"Día: {match.Groups[3].Value}");  // Salida: Día: 12
        }
    }
}
```

### Reemplazar Texto
El método `Regex.Replace` te permite reemplazar texto en una cadena que coincide con un patrón especificado.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Salida: Visit Google!
    }
}
```

### Dividir Cadenas
Puedes dividir una cadena en un arreglo basado en un patrón de regex usando el método `Regex.Split`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "uno,dos,tres,cuatro,cinco";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Salida:
        // uno
        // dos
        // tres
        // cuatro
        // cinco
    }
}
```

### Utilizar Bibliotecas de Terceros
Aunque el .NET Framework proporciona un amplio soporte para expresiones regulares, también existen bibliotecas de terceros como `PCRE.NET` que ofrecen expresiones regulares compatibles con Perl (PCRE) en C#. Esto puede ser útil si necesitas características o sintaxis del motor de regex de Perl que no están disponibles en la implementación de .NET.

Para usar `PCRE.NET`, primero instalarías su paquete NuGet, y luego puedes usarlo de manera similar a cómo usas las clases nativas de regex de .NET.

```csharp
// Ejemplo utilizando PCRE.NET aquí
// Nota: Imagina un ejemplo similar a los anteriores, adaptado para mostrar una característica única de PCRE.NET.
```

Al integrar bibliotecas de terceros para expresiones regulares, siempre consulta su documentación para obtener información detallada sobre el uso y compatibilidad.

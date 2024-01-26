---
title:                "Buscando y reemplazando texto"
date:                  2024-01-20T17:57:36.001656-07:00
model:                 gpt-4-1106-preview
simple_title:         "Buscando y reemplazando texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Buscar y reemplazar texto es una operación común que intercambia un fragmento de texto por otro. Los programadores lo hacen para corregir errores, actualizar datos o modificar código de forma masiva, lo cual ahorra tiempo y evita errores manuales.

## Cómo:
Aquí tienes un ejemplo básico en C#. Vamos a buscar y reemplazar texto en una cadena.

```C#
using System;

class Program
{
    static void Main()
    {
        string textoOriginal = "Hola mundo! Hola programadores!";
        string textoBuscado = "Hola";
        string textoReemplazo = "Adiós";
        
        string resultado = textoOriginal.Replace(textoBuscado, textoReemplazo);
        
        Console.WriteLine(resultado);  // Salida: Adiós mundo! Adiós programadores!
    }
}
```

También puedes usar expresiones regulares para reemplazos más complejos:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string textoOriginal = "La temperatura en Bogotá es de 20°C, en Medellín es de 24°C.";
        string patron = @"(\d+)°C";  // Dígitos seguidos de °C
        string reemplazo = @"($1°F)";
        
        string resultado = Regex.Replace(textoOriginal, patron, m => 
            ((int)(int.Parse(m.Groups[1].Value) * 1.8 + 32)).ToString() + "°F");
        
        Console.WriteLine(resultado); // Salida: La temperatura en Bogotá es de (68°F), en Medellín es de (75°F).
    }
}
```

## Profundización:
**Contexto histórico**: La función de buscar y reemplazar existe desde los primeros días de la edición de texto en computadoras, evolucionando desde comandos simples en editores de texto a operaciones complejas dentro de entornos de programación.

**Alternativas**: Además de `String.Replace` y `Regex.Replace` en C#, existen bibliotecas y herramientas como sed en Unix o Find and Replace en editores de texto que ofrecen funcionalidades similares para diferentes contextos.

**Detalles de implementación**: Mientras `String.Replace` es excelente para reemplazos directos, `Regex.Replace` permite un control detallado mediante patrones de coincidencia. Evalúa el rendimiento y la complejidad de las expresiones regulares, pues pueden ralentizar tu aplicación si se usan incorrectamente.

## Véase También:
- [Documentación oficial de Microsoft sobre String.Replace](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-7.0)
- [Tutorial de expresiones regulares en C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Herramienta en línea para probar expresiones regulares](https://regexr.com/)
- [Referencia sobre sed](https://www.gnu.org/software/sed/manual/sed.html)

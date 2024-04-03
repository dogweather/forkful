---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:06.106282-07:00
description: "C\xF3mo hacerlo: C# ofrece un enfoque directo para capitalizar cadenas\
  \ usando m\xE9todos incorporados. La forma m\xE1s simple de lograr esto es modificando\u2026"
lastmod: '2024-03-13T22:44:59.061824-06:00'
model: gpt-4-0125-preview
summary: "C# ofrece un enfoque directo para capitalizar cadenas usando m\xE9todos\
  \ incorporados."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
C# ofrece un enfoque directo para capitalizar cadenas usando métodos incorporados. La forma más simple de lograr esto es modificando directamente la cadena con estos métodos. Para reglas de capitalización más complejas o específicas (por ejemplo, capitalizar cada palabra), podrían ser necesarias bibliotecas adicionales o métodos manuales. A continuación, se muestran ejemplos que demuestran cómo capitalizar una cadena de diversas maneras en C#.

### Capitalización Básica:
Para capitalizar la primera letra de una sola palabra o frase:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Salida: "Hello world"
```

### Capitalizar Cada Palabra:
Para capitalizar la primera letra de cada palabra en una cadena, puedes usar el método `TextInfo.ToTitleCase` que se encuentra en el espacio de nombres `System.Globalization`:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Salida: "Hello World"
```

Nota: `ToTitleCase` no cambia a minúsculas el resto de las letras; solo cambia a mayúsculas la primera letra de cada palabra. Además, ciertas palabras en las reglas de mayúsculas de título (como "and", "or", "of") pueden no ser capitalizadas dependiendo de la configuración cultural.

### Usando Métodos de Extensión para la Reutilización:
Puedes crear un método de extensión para la clase `string` para simplificar el proceso de capitalización, haciendo que tu código sea más limpio y reutilizable. Así es como puedes crear y usar tal método:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Salida: "Hello world"
    }
}
```

Este método de extensión `Capitalize` se puede llamar en cualquier objeto de cadena dentro del espacio de nombres, ofreciendo un enfoque más intuitivo y orientado a objetos para la manipulación de cadenas en C#.

### Bibliotecas de Terceros:
Aunque la biblioteca estándar de C# cubre la mayoría de las necesidades para la capitalización de cadenas, ciertas tareas especializadas podrían beneficiarse de bibliotecas de terceros, como Humanizer. Sin embargo, para la tarea de simplemente capitalizar cadenas o cada palabra en una cadena, los métodos estándar de C# son adecuados y eficientes, negando la necesidad de dependencias externas.

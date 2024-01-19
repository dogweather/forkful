---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Eliminar caracteres que coinciden con un patrón es una acción que se usa para limpiar cadenas de caracteres no deseados. Los programadores recurren a esta técnica para manejar datos limpios y más eficientes.

## ¿Cómo se hace?

Usaremos un simple código de ejemplo para mostrar cómo se puede hacer esto.

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string pattern = "[^0-9]";
        string input = "ABC123";

        string replaced = Regex.Replace(input, pattern, "");

        Console.WriteLine(replaced);
    }
}
```
El código anterior producirá la siguiente salida:

```C#
123
```

Notarás que todas las letras fueron eliminadas, dejando solo los números.

## En Detalle

En el pasado, los métodos alternativos a menudo implicaban emplear bucles para iterar a través de cada carácter en la cadena. Esto resultaba ser tanto poco eficiente como visualmente poco atractivo dentro del código.

Ahora, con la clase `Regex` en C#, los programadores pueden utilizar potentes expresiones regulares. Esto no solo permite patrones más complejos, sino también una ejecución mucho más eficiente.

Una alternativa a este enfoque puede ser el uso del método `String.Replace`, pero es importante tener en cuenta que este método solo puede reemplazar exactamente las cadenas de caracteres que se le dan. Esto es mucho menos flexible que una expresión regular que permite patrones de coincidencia.

En los detalles de la implementación, `Regex.Replace(input, pattern, "");` es la línea donde realmente se realiza la eliminación de caracteres. Aquí, todos los caracteres del `input` que coinciden con el `pattern` son reemplazados por una cadena vacía.

## Ver También

Puedes profundizar en las expresiones regulares de C# en la [documentación oficial de Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-5.0).

Para más información sobre el manejo de cadenas en C#, consulta [este artículo](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/) en Microsoft Docs.
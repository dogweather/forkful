---
title:                "Capitalizando una cadena de texto"
html_title:           "C#: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Capitalizar una cadena en programación significa convertir el primer carácter de la cadena a una letra mayúscula. Los programadores lo hacen para mejorar la legibilidad del texto y para dar formato a las entradas del usuario.

## Cómo hacer:
A continuación, te muestro cómo capitalizar una cadena en C# usando el método `TextInfo.ToTitleCase()` de la clase `CultureInfo`.

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        var cultureInfo = new CultureInfo("en-US");
        var textInfo = cultureInfo.TextInfo;

        string myString = "esta es una frase de ejemplo";
        string titleCase = textInfo.ToTitleCase(myString);
        Console.WriteLine(titleCase); 
    }
}
```

El resultado de este código será:

```C#
Esta Es Una Frase De Ejemplo
```

## Análisis en profundidad:
Históricamente, C# no ha ofrecido un método nativo para capitalizar una cadena. Como alternativa, algunos desarrolladores optaban por escribir sus propios métodos. Sin embargo, con la clase `CultureInfo`, capitalizar una cadena es ahora una tarea sencilla.

Debido a su implementación, `TextInfo.ToTitleCase()` no cambia las palabras que ya están en mayúsculas. Si necesitas asegurarte de que todo sea mayúsculas, deberías convertir toda la cadena a minúsculas antes de aplicar `ToTitleCase()`.

```C#
string myString = "ME GUSTA C#";
myString = myString.ToLower();
string titleCase = textInfo.ToTitleCase(myString);
```

Con este código, el resultado será: `Me Gusta C#`.

## Ver también:
Para obtener más información sobre el tratamiento de cadenas en C#, visita los siguientes enlaces:

- Método ToTitleCase: [https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)
- Clase CultureInfo: [https://docs.microsoft.com/es-es/dotnet/api/system.globalization.cultureinfo?view=net-5.0](https://docs.microsoft.com/es-es/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
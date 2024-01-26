---
title:                "Uso de expresiones regulares"
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Usar expresiones regulares es como pescar palabras o patrones en un mar de texto. Los programadores las usan porque son potentes y precisas para buscar, validar o manipular datos en cadenas de texto.

## How to:
Para trabajar con expresiones regulares en C#, necesitas incluir el espacio de nombres `System.Text.RegularExpressions`. Aquí hay un ejemplo simple para validar un correo electrónico:

```c#
using System;
using System.Text.RegularExpressions;

class Program {
    static void Main() {
        string emailPattern = @"^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$";
        string emailToTest = "hola@example.com";

        bool isValidEmail = Regex.IsMatch(emailToTest, emailPattern);
        
        Console.WriteLine(isValidEmail ? "Email válido" : "Email inválido");
    }
}
```
Salida esperada: `Email válido`

## Deep Dive
Las expresiones regulares tienen raíces en la teoría de autómatas y lenguajes formales. Alternativamente, para buscar sin regex, puedes usar métodos como `Contains`, `StartsWith` o `EndsWith`. Pero las regex ofrecen una flexibilidad inigualable. Recuerda que son costosas en cuanto a rendimiento, así que úsalas solo cuando sea necesario.

## See Also
Aquí tienes algunas fuentes útiles para profundizar en las expresiones regulares:
- Documentación de Microsoft sobre `System.Text.RegularExpressions`: https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex
- Tutorial interactivo de Regex: https://regexone.com/
- Herramienta para probar tus expresiones regulares: https://regexr.com/

---
title:                "Capitalizando una cadena"
html_title:           "C#: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En la programación, capitalizar una cadena significa convertir la primera letra de cada palabra en mayúscula. Los programadores lo hacen para mejorar la legibilidad y organización de su código.

## Cómo:

Para capitalizar una cadena en C#, puedes utilizar el método `ToTitleCase()` de la clase `TextInfo`. Este método tomará una cadena como parámetro y devolverá una nueva cadena con la primera letra de cada palabra en mayúscula.

```C#
string miCadena = "hola, ¿cómo estás?";
var miCadenaCapitalizada = new CultureInfo("es-ES",false).TextInfo.ToTitleCase(miCadena);

Console.WriteLine(miCadenaCapitalizada);
//Salida: Hola, ¿Cómo Estás?
```

## Inmersión Profunda:

Antes, capitalizar una cadena en C# requirió una gran cantidad de código y lógica. Sin embargo, con el método `ToTitleCase()` introducido en .NET Framework 2.0, se simplificó el proceso. También existen alternativas a la capitalización, como convertir toda la cadena a mayúsculas o sólo la primera letra. Es importante tener en cuenta las especificaciones de la cultura en la que se está trabajando al utilizar el método `ToTitleCase()`, ya que puede haber diferencias en la capitalización entre idiomas.

## Ver También:

- [Método `ToTitleCase()` en la documentación de Microsoft](https://docs.microsoft.com/es-es/dotnet/api/system.globalization.textinfo.totitlecase?view=netframework-4.8)
- [Especificaciones de cultura en C#](https://docs.microsoft.com/es-es/dotnet/api/system.globalization.cultureinfo?view=netframework-4.8)
---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Capitalizar un texto en programación significa convertir todas las letras de una cadena a mayúscula. Los programadores lo hacen para normalizar los datos, mejorar la legibilidad o cumplir con requisitos específicos, como los nombres propios en una interfaz de usuario.

## Cómo hacerlo:
Vamos directo al grano. Aquí tienen cómo capitalizar una cadena en C#:

```C#
using System;

class CapitalizeString
{
    static void Main()
    {
        string original = "hola mundo";
        string capitalized = original.ToUpper();

        Console.WriteLine(capitalized);
    }
}
```

Salida esperada:

```
HOLA MUNDO
```

Otra opción es usar `CultureInfo` para manejar casos específicos de idioma:

```C#
using System;
using System.Globalization;

class CapitalizeStringCulture
{
    static void Main()
    {
        string original = "hola mundo";
        CultureInfo cultureInfo = new CultureInfo("es-ES");
        string capitalized = original.ToUpper(cultureInfo);

        Console.WriteLine(capitalized);
    }
}
```

Salida esperada:

```
HOLA MUNDO
```

## Profundizando
En la antigüedad, C# no era tan flexible con la manipulación de cadenas, pero con cada versión se han agregado herramientas más poderosas. Además de `string.ToUpper()`, podríamos considerar `TextInfo.ToTitleCase()` si quisiéramos capitalizar solo las iniciales de cada palabra. Esto es útil al tratar con nombres propios o títulos.

```C#
using System;
using System.Globalization;

class TitleCaseExample
{
    static void Main()
    {
        string original = "hola mundo";
        TextInfo textInfo = new CultureInfo("es-ES", false).TextInfo;
        
        string titleCase = textInfo.ToTitleCase(original);

        Console.WriteLine(titleCase);
    }
}
```

Salida esperada:

```
Hola Mundo
```

Dependiendo del idioma de la cultura que se esté utilizando, la capitalización puede comportarse de forma distinta. Por ejemplo, algunas culturas no utilizan mayúsculas y minúsculas de la misma forma que el español o el inglés.

## Ver También
Para expandir tus conocimientos, revisa las siguientes fuentes:

- [Documentación oficial de `String.ToUpper()` en Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Información Cultural en .NET (`CultureInfo`)](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
- [`TextInfo.ToTitleCase()` en Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)
- [Guía de Estilos de Capitalización para Interfaz de Usuario](https://docs.microsoft.com/en-us/style-guide/capitalization)

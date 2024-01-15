---
title:                "Cambiando la primera letra de cada palabra a mayúscula en un texto"
html_title:           "C#: Cambiando la primera letra de cada palabra a mayúscula en un texto"
simple_title:         "Cambiando la primera letra de cada palabra a mayúscula en un texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en C#?

En C#, hay veces en las que necesitamos capitalizar una cadena de texto, es decir, convertir la primera letra de cada palabra en mayúscula. Esto puede ser útil en situaciones como formateo de nombres o para mejorar la legibilidad de una cadena en un programa.

## Cómo hacerlo en C#

Hay varias formas de capitalizar una cadena en C#. Una de las formas más sencillas es utilizando el método `ToTitleCase` de la clase `TextInfo` de la librería `System.Globalization`. Aquí hay un ejemplo:

```C#
using System.Globalization;

string cadena = "hola mundo";

// utilizando ToTitleCase
TextInfo ti = new CultureInfo("es-ES", false).TextInfo;
string cadenaCapitalizada = ti.ToTitleCase(cadena);

// salida: Hola Mundo
Console.WriteLine(cadenaCapitalizada);
```

Otra forma es utilizando el método `Substring` y `ToUpper` de la clase `String`:

```C#
string cadena = "hola mundo";

// utilizando Substring y ToUpper
string primeraLetra = cadena.Substring(0, 1).ToUpper();
string restante = cadena.Substring(1).ToLower();
string cadenaCapitalizada = primeraLetra + restante;

// salida: Hola Mundo
Console.WriteLine(cadenaCapitalizada);
```

También es posible utilizar expresiones regulares para capitalizar una cadena. Aquí hay un ejemplo utilizando el método `Regex.Replace`:

```C#
using System.Text.RegularExpressions;

string cadena = "hola mundo";

// utilizando Regex.Replace
string cadenaCapitalizada = Regex.Replace(cadena, @"(^\w)|(\s\w)", m => m.Value.ToUpper());

// salida: Hola Mundo
Console.WriteLine(cadenaCapitalizada);
```

## Un poco más a fondo

La razón por la que utilizamos `ToTitleCase` de la clase `TextInfo` es porque tiene en cuenta las reglas de capitalización de cada idioma. Esto significa que en un idioma como el español, las palabras como "de", "el", "los" no serán convertidas a mayúscula, ya que no son palabras que se capitalicen en una oración. Mientras que en un idioma como el inglés, sí lo son.

Además, al utilizar `ToTitleCase`, también tenemos en cuenta caracteres especiales como tildes o diéresis, lo que nos asegura que la cadena sea capitalizada de manera correcta en cualquier idioma.

## Ver también

- [Documentación oficial de Microsoft sobre el método `ToTitleCase`](https://docs.microsoft.com/es-es/dotnet/api/system.globalization.textinfo.totitlecase?view=netcore-3.1)
- [Documentación oficial de Microsoft sobre los métodos `Subrstring` y `ToUpper`](https://docs.microsoft.com/es-es/dotnet/api/system.string.substring?view=netcore-3.1)
- [Documentación oficial de Microsoft sobre el método `Regex.Replace`](https://docs.microsoft.com/es-es/dotnet/api/system.text.regularexpressions.regex.replace?view=netcore-3.1)
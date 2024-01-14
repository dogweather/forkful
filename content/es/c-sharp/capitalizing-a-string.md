---
title:                "C#: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación en C#, a menudo nos encontramos con la necesidad de capitalizar una cadena de texto. Esto significa convertir la primera letra de cada palabra en mayúscula. ¿Pero por qué querríamos hacer esto? Hay varias razones posibles: estandarizar la presentación del texto, mejorar la legibilidad para el usuario, o simplemente por preferencia personal.

## Cómo hacerlo

Afortunadamente, en C# hay varias formas de capitalizar una cadena. Una opción es utilizar el método `ToUpper()` de la clase `Char`, que convierte un solo carácter en mayúscula. Sin embargo, para capitalizar todas las palabras en una cadena, es más conveniente utilizar el método `ToTitleCase()` de la clase `TextInfo`, que se encuentra en el espacio de nombres `System.Globalization`.

A continuación se muestra un ejemplo de código que utiliza `ToTitleCase()`:

```C#
using System;
using System.Globalization;

string texto = "este es un ejemplo de cadena";
TextInfo ti = new CultureInfo("es-ES",false).TextInfo;
string capitalizado = ti.ToTitleCase(texto);
Console.WriteLine(capitalizado);
```

El resultado de este código sería:

```
Este Es Un Ejemplo De Cadena
```

Otra opción es utilizar el método `ToUpperInvariant()` de la clase `String`, que convierte todos los caracteres en mayúscula siguiendo las reglas invariantes del sistema, independientemente del idioma. Este método es útil para aplicaciones multilingües.

## Profundizando

Para aquellos interesados en entender más a fondo cómo funciona la capitalización de cadenas en C#, es importante tener en cuenta que el método `ToTitleCase()` se basa en las reglas culturales de la configuración regional actual del sistema. Por lo tanto, si se cambia la configuración regional, el resultado de la capitalización también puede cambiar.

Además, debemos tener en cuenta que algunas palabras tienen letras con acentos o diacríticos que pueden no ser capitalizadas correctamente por defecto. Por ejemplo, si se quiere capitalizar la palabra "árbol", el resultado sería "ÁRbol". Para solucionar esto, podemos utilizar el método `ToUpper()` junto con la clase `StringNormalization` para eliminar los diacríticos antes de capitalizar la cadena.

## Ver también

- Documentación oficial de Microsoft sobre el método `ToTitleCase()`: https://docs.microsoft.com/es-es/dotnet/api/system.globalization.textinfo.totitlecase?view=netframework-4.8
- Blog post en español sobre capitalización de cadenas en C#: https://www.campusmvp.es/recursos/post/C-sharp-capitalizacion-de-texto.aspx
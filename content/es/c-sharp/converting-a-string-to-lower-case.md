---
title:                "C#: Convirtiendo un string a minúsculas"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

El convertir una cadena de texto a minúsculas es una tarea común en la programación en C#. Al hacerlo, podemos asegurarnos de que los datos se almacenen y manejen de manera consistente, lo que facilita su posterior uso en comparaciones y búsquedas. Además, muchos componentes de la plataforma .NET, como bases de datos y servicios web, asumen que las cadenas están en minúsculas, por lo que es importante convertirlas correctamente para evitar problemas en nuestro código.

## Cómo hacerlo

La forma más simple de convertir una cadena a minúsculas en C# es utilizando el método `ToLower()` del objeto `string`. Este método devuelve una nueva cadena con todos los caracteres convertidos a minúsculas. Veamos un ejemplo:

```C#
string cadena = "ESTA ES UNA CADENA EN MAYÚSCULAS";
string cadenaMinusculas = cadena.ToLower();

Console.WriteLine(cadenaMinusculas);
```
**Output:** esta es una cadena en mayúsculas

Además de `ToLower()`, también podemos utilizar `ToLowerInvariant()`, que realiza la conversión de minúsculas utilizando reglas de cultura invariantes, lo que puede ser útil en aplicaciones multilingües.

Existen otras formas de convertir cadenas a minúsculas, como utilizar la clase `TextInfo` del namespace `System.Globalization` o utilizar expresiones regulares. Sin embargo, `ToLower()` sigue siendo la forma más eficiente y fácil de realizar esta tarea.

## Un paso más allá

Ahora que conocemos la forma básica de convertir cadenas a minúsculas, podemos profundizar un poco más. ¿Qué pasa si solo queremos convertir la primera letra de una cadena a minúsculas y dejar el resto en su formato original? En ese caso, podemos utilizar el método `ToLower()` junto con `Substring()` para lograrlo.

Supongamos que tenemos una cadena con el nombre de una persona en mayúsculas y queremos mostrarla en el formato adecuado de nombre propio, con la primera letra en minúsculas. Podríamos hacerlo de la siguiente manera:

```C#
string nombre = "JUAN";
string nombrePropio = nombre[0].ToString().ToLower() + nombre.Substring(1);

Console.WriteLine(nombrePropio);
```
**Output:** Juan

También podemos utilizar esta técnica para convertir la primera letra de cada palabra en una cadena a minúsculas, por ejemplo, en una oración.

## Ver también

- [Método ToLower() en Microsoft Docs](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolower?view=net-5.0)
- [Clase TextInfo en Microsoft Docs](https://docs.microsoft.com/es-es/dotnet/api/system.globalization.textinfo?view=net-5.0)
- [Cadenas en C# en la Wikipedia](https://es.wikipedia.org/wiki/Cadena_de_caracteres_en_C_Sharp)
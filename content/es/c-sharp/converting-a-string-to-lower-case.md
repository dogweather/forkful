---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La conversión de una cadena a minúsculas implica cambiar todas las letras mayúsculas en una cadena a su equivalente en minúsculas. Los programadores hacen esto para realizar comparaciones de cadenas sin diferencias de mayúsculas y minúsculas, o para normalizar los datos de entrada del usuario.

## Cómo hacerlo:

Aquí está el código de cómo hacerlo en C#. Es simple y directo gracias al método `ToLower()`. Aquí hay un ejemplo de cómo se ve.

```C#
string str = "Hola Mundo!";
string lowerStr = str.ToLower();
Console.WriteLine(lowerStr);
```

El resultado sería:

```C#
"hola mundo!"
```

Muy sencillo, ¿verdad? Lo es.

## Inmersión Profunda:

La función `ToLower()` ha sido una parte integral de C# desde sus primeras versiones debido a la necesidad de realizar operaciones de cadenas insensibles a mayúsculas y minúsculas.

Alternativamente, podría utilizar el método `ToLowerInvariant()` si está trabajando con culturas diferentes a las del inglés. Este método se utiliza para obtener resultados consistentes independientemente de la configuración regional del sistema.

En su implementación, `ToLower()` convierte cada carácter en la cadena llamada a su equivalente en minúsculas utilizando las reglas de mayúsculas y minúsculas de la cultura actual.

## Ver También:

- Documentación de Microsoft sobre ToLower(): [link](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolower?view=net-5.0)
- Pila de desbordamiento: diferencia entre ToLower() y ToLowerInvariant(): [link](https://stackoverflow.com/questions/6225808/difference-between-tolower-and-tolowerinvariant)
- Documentación de Microsoft sobre ToLowerInvariant(): [link](https://docs.microsoft.com/es-es/dotnet/api/system.string.tolowerinvariant?view=net-5.0)

Por favor, siga estudiando y practicando. ¡Sigue programando!
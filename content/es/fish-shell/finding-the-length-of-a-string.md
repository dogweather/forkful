---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Encontrar la longitud de una cadena se trata de determinar el número de caracteres en esa cadena. Los programadores lo hacen para muchas razones como validar la entrada del usuario, cortar partes de la cadena, y comparar cadenas.

## Cómo hacerlo:

En Fish Shell, encontrar la longitud de una cadena es bastante directo. Aquí tienes un ejemplo simple:

```Fish Shell
set cadena "Hola Mundo"
echo (string length $cadena)
```

La salida sería:

```Fish Shell
10
```

## Análisis Detallado

Si bien la función `string length` es una adición relativamente nueva a Fish Shell (se introdujo en la versión 2.3.0), rápidamente se ha convertido en una de las operaciones más comunes en la manipulación de cadenas.

Una alternativa a usar `string length` sería recorrer la cadena carácter por carácter e incrementar un contador, pero esto es más lento y requiere más líneas de código.

En términos de implementación, `string length` usa la biblioteca de cadenas de caracteres wide de C para contar el número de caracteres en la cadena, lo que significa que también maneja correctamente los caracteres Unicode.

## Ver También

Estos recursos te pueden ser útiles si quieres aprender más acerca de cómo trabajar con cadenas en Fish Shell:

- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Manual de operación de cadenas en Fish Shell](https://fishshell.com/docs/current/cmds/string.html)
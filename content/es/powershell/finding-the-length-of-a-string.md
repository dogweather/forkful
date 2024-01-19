---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hola a todos. En este artículo les mostraré cómo encontrar la longitud de una cadena en PowerShell. ¡Vamos a ello!

## ¿Qué y por qué?
La longitud de una cadena es la cantidad de caracteres que contiene. Los programadores a menudo necesitan esta información para validar la entrada del usuario, iterar a través de los caracteres de una cadena, entre otras tareas.

## ¿Cómo hacerlo?
En PowerShell, encontrar la longitud de una cadena es sencillo. Solo necesitas usar la propiedad `.Length` después de la cadena. Aquí hay un ejemplo.

```PowerShell
$cadena = "Hola, Mundo"
$cadena.Length
```

Esto imprimirá `11`, que es la cantidad de caracteres en la cadena, incluyendo los espacios y los comas.

## Inmersión Profunda
La propiedad `.Length` ha sido parte de PowerShell desde su primera versión. Alternativamente, puedes usar el método `Measure-Object -Character` que proporcionará la misma longitud de la cadena. Sin embargo, `.Length` es más directo y rápido.

La propiedad `.Length` no incluye caracteres ocultos o de control, solo aquelos que puedes ver e imprimir. Si necesitas considerar esos caracteres, debes aplicar técnicas de manejo de cadenas más avanzadas.

## Ver También
Para obtener más información sobre el manejo de cadenas en PowerShell, visita estos recursos:
1. Documentación oficial de Microsoft sobre cadenas en PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1
2. Un análisis completo sobre cadenas en PowerShell por Adam Bertram: https://adamtheautomator.com/powershell-string-length/ 

¡Y eso es todo! Ahora ya sabes cómo encontrar la longitud de una cadena en PowerShell. ¡Hasta la próxima!
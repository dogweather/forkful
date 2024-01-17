---
title:                "Interpolando una cadena"
html_title:           "PowerShell: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Interpolar una cadena de texto es cuando incorporamos una variable dentro de una cadena para generar un resultado personalizado. Los programadores lo hacen para ahorrar tiempo y crear cadenas dinámicas que se ajusten a diferentes situaciones.

## Cómo hacerlo:

Para interpolar una cadena en PowerShell, simplemente coloca la variable entre llaves `{ }` dentro de la cadena. Por ejemplo:

```PowerShell
$nombre = "Juan"
Write-Host "¡Hola {nombre}! Bienvenido a mi programa."
```

La salida sería: `¡Hola Juan! Bienvenido a mi programa.`

Si quieres incorporar una expresión en la cadena, utiliza el signo de dólar `$` antes de la expresión. Por ejemplo:

```PowerShell
$edad = 25
Write-Host "Tienes $($edad + 5) años."
```

La salida sería: `Tienes 30 años.`

## Profundizando:

Interpolar cadenas ha sido una técnica común en muchos lenguajes de programación desde hace décadas. Antes de PowerShell, los desarrolladores tenían que utilizar concatenación para lograr el mismo resultado. En PowerShell, también puedes utilizar subexpresiones, combinando dos expresiones en una sola. Alternativas a la interpolación de cadenas pueden ser el uso de formatos de cadena, las cadenas heredoc, o la concatenación.

## Ver también:

Si quieres aprender más sobre el uso de cadenas en PowerShell, puedes visitar el sitio oficial de Microsoft: 
https://docs.microsoft.com/en-us/powershell/scripting/core-powershell/strings-and-formatting?view=powershell-7.1

Y para ver cómo se utiliza el concepto de interpolación en otros lenguajes de programación, puedes consultar este artículo: 
https://www.geeksforgeeks.org/string-interpolation-python/
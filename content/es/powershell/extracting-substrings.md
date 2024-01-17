---
title:                "Extrayendo subcadenas"
html_title:           "PowerShell: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

La extracción de subcadenas se refiere a la acción de obtener una porción específica de una cadena de texto más grande. Los programadores a menudo lo hacen para manipular y trabajar con una parte específica de una cadena en lugar de toda ella.

## Cómo:

Puedes extraer subcadenas en PowerShell utilizando el comando `Select-String` y el operador `Substring`. Aquí hay un ejemplo:

```
$myString = "Hola amigos!"
$substring = $myString.Substring(5, 7)
# Output: amigos!
```

## Profundizando:

La extracción de subcadenas ha sido una técnica popular y común en la programación desde los primeros días. Alternativas a la extracción de subcadenas incluyen el uso de expresiones regulares y funciones específicas del lenguaje de programación.

Al utilizar el operador `Substring`, es importante recordar que el primer índice comienza en 0 y no en 1, y que también puedes proporcionar un valor negativo para obtener una subcadena a partir del final de la cadena original.

## Consulta también:

Puedes encontrar más información sobre la extracción de subcadenas en la [documentación oficial de Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netcore-3.1) o en [este tutorial en español](https://www.jesusninoc.com/03/01/extract-string-from-a-text-file-in-powershell/).
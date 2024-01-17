---
title:                "Mayúsculas de una cadena"
html_title:           "PowerShell: Mayúsculas de una cadena"
simple_title:         "Mayúsculas de una cadena"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Capitalize una cadena de texto es simplemente convertirla a mayúsculas. Los programadores a menudo lo hacen para normalizar y estandarizar el formato del texto, lo que facilita su manipulación y comparación.

## ¿Cómo hacerlo?

Utilizar el comando ```.ToUpper()``` en PowerShell es la forma más simple y directa de capitalizar una cadena. Por ejemplo: 

```
$cadena = "hola mundo"
$cadena.ToUpper()
```

Este código producirá la salida "HOLA MUNDO". 

## Inmersión profunda

La capitalización de cadenas de texto ha sido un tema importante en el desarrollo de lenguajes de programación. Antes del uso de las computadoras, los teletipos y las máquinas de escribir solo podían imprimir letras mayúsculas, por lo que era necesario cambiar al modo de mayúsculas para resaltar texto. Sin embargo, en la era de las computadoras, esto ya no era necesario y los lenguajes de programación comenzaron a dar opciones para manejar las cadenas de texto en mayúsculas o minúsculas. Alternativas a la función ```.ToUpper()``` incluyen ```.ToLower()``` y ```.ToTitleCase()```.

## Ver también

- Documentación de Microsoft para .ToUpper() en PowerShell: https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netframework-4.8
- Artículo de blog sobre la historia de la capitalización de cadenas: https://stackify.com/quick-history-capitalizing-strings/
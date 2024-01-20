---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Convertir una cadena a minúsculas significa cambiar todas las letras del texto a minúsculas. Los programadores lo hacen para uniformizar datos, especialmente cuando la comparación y clasificación de strings es de importancia.

## Cómo hacerlo:

Aquí te muestro cómo convertir un string a minúsculas con PowerShell. El comando a utilizar es `.ToLower()`. Aquí tienes un ejemplo:

```PowerShell
$str = 'Hola Mundo!'
$strMinusculas = $str.ToLower()
echo $strMinusculas
```

Este script imprimirá:

```PowerShell
hola mundo!
```

## Buceo Profundo:

Historia: Los métodos `ToLower` y `ToUpper` se introdujeron inicialmente en .NET Framework, del cual PowerShell es una interfaz de scripting.

Alternativas: No hay muchas alternativas a `ToLower()` en PowerShell. Aunque puedes utilizar métodos de .NET Framework como `String.ToLowerInvariant()`, en la mayoría de los casos, `ToLower()` será suficiente.

Implementación: `ToLower()` es un método de los objetos de string en PowerShell, respaldado por la implementación de .NET Framework. Convierte cada carácter alfabético en el string a su equivalente en minúsculas.

## Ver También:

Para más información, visita estas páginas web:

- [String.ToLower Method (System) | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)



No dudes en explorar y aprender más acerca de la manipulación de strings en PowerShell.
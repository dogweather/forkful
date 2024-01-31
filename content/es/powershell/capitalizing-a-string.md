---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar un texto significa convertir la primera letra de cada palabra en mayúsculas. Los programadores lo hacen para mejorar la legibilidad y para formatear la salida de datos de manera consistente.

## Cómo hacerlo:

Aquí tienes varios ejemplos de cómo capitalizar strings en PowerShell:

```PowerShell
# Capitalizing each word in a string
$texto = "hola mundo, estoy programando en PowerShell"
$textoCapitalizado = $texto | ForEach-Object { $_.ToTitleCase() }
Write-Output $textoCapitalizado
# Salida: Hola Mundo, Estoy Programando En PowerShell

# Capitalizing the first letter of a string
$texto = "esto es un ejemplo"
$primerCaracterEnMayuscula = $texto.Substring(0,1).ToUpper() + $texto.Substring(1)
Write-Output $primerCaracterEnMayuscula
# Salida: Esto es un ejemplo

# Uso de métodos de .NET para capitalizar
[System.Globalization.TextInfo]$textInfo = [System.Globalization.CultureInfo]::CurrentCulture.TextInfo
$texto = "otro ejemplo para powershell"
$textoCapitalizado = $textInfo.ToTitleCase($texto)
Write-Output $textoCapitalizado
# Salida: Otro Ejemplo Para Powershell
```

## Profundización:

Históricamente, capitalizar texto ha sido importante tanto en la escritura como en la programación. Antes de PowerShell y otros lenguajes modernos, manipular strings requería lógica más compleja. En PowerShell, gracias al acceso a métodos .NET, capitalizar strings es sencillo y versátil.

Además del método `ToTitleCase()`, puedes utilizar métodos personalizados o expresiones regulares para resultados específicos. La elección del método depende de tus necesidades: puede que necesites capitalizar solo la primera letra de una frase o cada palabra en un título.

En cuanto a implementación, es crucial entender que `ToTitleCase()` no cambiará palabras en mayúsculas a minúsculas, excepto la primera letra. También, las culturas difieren en cómo manejan la capitalización; por ejemplo, algunas no capitalizan después de ciertos caracteres.

## Ver También:

- [Culturas y sus diferencias con TextInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo?view=net-6.0)

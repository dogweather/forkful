---
title:                "Capitalizando una cadena de caracteres"
html_title:           "PowerShell: Capitalizando una cadena de caracteres"
simple_title:         "Capitalizando una cadena de caracteres"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La capitalización de una cadena consiste en cambiar la primera letra de cada palabra a mayúscula. Los programadores lo hacen para mejorar la legibilidad y la presentación de los textos.

## ¿Cómo hacerlo?

```PowerShell
# Ejemplo de entrada
$cadena = "hola mundo"

# Uso de la función ToTitleCase
$cadenaCapitalizada = (Get-Culture).TextInfo.ToTitleCase($cadena)

# Visualización de la salida
Write-Host $cadenaCapitalizada
```
El resultado será:

```PowerShell
Hola Mundo
```
## Profundización 

En el pasado, se tenía que hacer una función personalizada para capitalizar las cadenas en PowerShell, ya que esta característica no estaba integrada en las versiones anteriores. Pero con el tiempo, la función ToTitleCase se implementó en .NET y PowerShell la adoptó al ser un shell basado en .NET.

Una alternativa para capitalizar una cadena en PowerShell es utilizando el operador `-replace` con una expresión regular.

```PowerShell
$cadenaCapitalizada = $cadena -replace '\b\w', { $_.Value.ToUpper() }
```

Tenga en cuenta que la función `ToTitleCase` de .NET no cambia a minúsculas las letras que están en mayúsculas al principio, solo cambia a mayúsculas las primeras letras en minúsculas. Eso significa que si tienes "HOLA MUNDO", la salida será la misma, "HOLA MUNDO". Si esto te preocupa, podrías convertir toda la cadena a minúsculas antes de aplicar `ToTitleCase`.

## Ver También

Visita estos enlaces para obtener más información relacionada:

- PowerShell String Manipulation: https://ss64.com/ps/syntax-operators.html
- .NET TextInfo.ToTitleCase: https://msdn.microsoft.com/en-us/library/system.globalization.textinfo.totitlecase
- Regex in PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions
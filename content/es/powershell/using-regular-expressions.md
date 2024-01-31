---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
Las expresiones regulares son patrones usados para encontrar coincidencias de texto según reglas definidas. Los programadores las usan porque permiten buscar y manipular texto de manera eficiente y sofisticada.

## Cómo hacerlo:
Aquí te muestro cómo hacer coincidir y reemplazar texto usando expresiones regulares en PowerShell con los cmdlets `-match`, `-replace`, y `Select-String`.

### Encontrar coincidencias
```PowerShell
$texto = "Hola, mi número de teléfono es 123-456-7890."
$patron = '\d{3}-\d{3}-\d{4}'
if ($texto -match $patron) {
    "Coincidencia encontrada: " + $Matches[0]
} else {
    "No se encontró la coincidencia."
}
```
Salida: `Coincidencia encontrada: 123-456-7890`

### Reemplazar texto
```PowerShell
$textoReemplazado = $texto -replace $patron, 'XXX-XXX-XXXX'
$textoReemplazado
```
Salida: `Hola, mi número de teléfono es XXX-XXX-XXXX.`

### Extraer coincidencias con `Select-String`
```PowerShell
$lineas = Get-Content .\archivo.txt
$lineas | Select-String $patron
```
Este comando extraerá y mostrará las líneas del archivo `archivo.txt` que contengan el patrón definido.

## Profundizando
Las expresiones regulares tienen sus raíces en la teoría de autómatas y lenguajes formales. Alternativas al uso de regex incluyen el uso de funciones de texto simples o parsers específicos para la estructura de datos. En PowerShell, las expresiones regulares están implementadas a través del .NET Framework, lo que las hace muy potentes pero también pueden ser complejas para los principiantes.

## Ver También
- [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1) - Documentación oficial de PowerShell sobre expresiones regulares.
- [Regex class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex) - Documentación de la clase Regex de .NET.
- [Learn Regular Expressions](https://regexone.com/) - Recurso interactivo para aprender expresiones regulares desde cero.

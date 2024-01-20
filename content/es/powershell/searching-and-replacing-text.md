---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

---
## ¿Qué es & Por qué?
Buscar y reemplazar texto es una función común usada para localizar secuencias de caracteres específicas y sustituirlas por otras. Esta habilidad es esencial para los programadores porque agiliza la modificación de datos y códigos. 

## Cómo hacerlo
Supongamos que tienes un texto en el que quieres reemplazar todas las apariciones de "hola" por "adios". En PowerShell, puedes lograr esto fácilmente con el comando '-replace'. Este es un ejemplo de cómo funciona:

```PowerShell
$texto = "hola mundo, hola a todos"
$texto = $texto -replace 'hola', 'adios'
Write-Output $texto
```

Cuando ejecutes este script, veras lo siguiente:

```PowerShell
adios mundo, adios a todos
```
## Profundizando
El uso de la función de búsqueda y reemplazo ha marcado presencia desde los albores de la programación. 

En términos de alternativas, la función '-replace' específica de PowerShell es tan potente como cualquier otra en lenguajes de programación diferentes. Sin embargo, si buscas mayor flexibilidad, puedes utilizar expresiones regulares (regex) para patrones de búsqueda más complejos:

```PowerShell
$texto = "hola mundo 123, hola a todos 456"
$texto = $texto -replace 'hola (.*?)\d+', 'adios'
Write-Output $texto
```

Esta vez, veras:

```PowerShell 
adios mundo, adios a todos
```
La utilidad '-replace' implementa la coincidencia de expresiones regulares. El primer argumento es la expresión regular a buscar, y el segundo es el texto de reemplazo.

## Véase También
1. [Documentación oficial de PowerShell](https://docs.microsoft.com/es-es/powershell/)
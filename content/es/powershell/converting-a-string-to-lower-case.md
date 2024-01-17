---
title:                "Convirtiendo una cadena en minúsculas"
html_title:           "PowerShell: Convirtiendo una cadena en minúsculas"
simple_title:         "Convirtiendo una cadena en minúsculas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Convertir una cadena de texto a minúsculas es una tarea común en la programación. Esto se refiere a cambiar todas las letras mayúsculas de una cadena a letras minúsculas. Los programadores hacen esto para facilitar la comparación de cadenas y para asegurarse de que los datos ingresados por los usuarios estén correctamente formateados.

## ¿Cómo hacerlo?
```PowerShell
$string = "Hola Mundo"
Write-Host $string.ToLower()
```
Salida:
```PowerShell
hola mundo
```
El código anterior utiliza el método `ToLower()` para convertir la cadena `$string` a minúsculas. También puedes usar el operador `-replace` para reemplazar todas las letras mayúsculas con sus equivalentes en minúsculas:
```PowerShell
$string = "Hola Mundo"
Write-Host $string -replace "[A-Z]",{param($m) $m.Value.ToLower()}
```
Salida:
```PowerShell
hola mundo
```

## Profundizando
La conversión de cadenas a minúsculas existe desde los inicios de la programación. Sin embargo, con la llegada de diferentes lenguajes de programación y plataformas, también han surgido diferentes métodos para realizar esta tarea. En PowerShell, además del método `ToLower()`, también puedes usar el operador `-replace` mencionado anteriormente, o los métodos `ToLowerInvariant()` y `ToLowerInvariant()`. Además, puedes utilizar la clase `CultureInfo` para especificar el idioma en el que deseas realizar la conversión de minúsculas.

## Ver también
- [Documentación oficial de Microsoft sobre la clase `String`](https://docs.microsoft.com/es-es/dotnet/api/system.string?view=net-5.0)
- [Página web de información sobre la clase `CultureInfo`](https://stackoverflow.com/questions/1501716/how-do-i-translate-the-net-cultureinfo-object-into-a-culture-info-object-in-p)
---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con JSON significa manejar este formato ligero de intercambio de datos, muy usado por su facilidad de lectura y escritura. Los programadores lo utilizan para intercambiar información entre sistemas, configuraciones, y API debido a su estructura simple y universal.

## Cómo hacerlo:
```PowerShell
# Convertir de objeto PowerShell a JSON
$objeto = @{
  Nombre = 'Juan'
  Edad = 30
  EsProgramador = $true
}
$json = $objeto | ConvertTo-Json
Write-Output $json
```
Salida:
```json
{
  "Nombre": "Juan",
  "Edad": 30,
  "EsProgramador": true
}
```

```PowerShell
# Convertir de JSON a objeto PowerShell
$jsonString = '{"Nombre": "Juan", "Edad": 30, "EsProgramador": true}'
$objeto = $jsonString | ConvertFrom-Json
Write-Output $objeto.Nombre
```
Salida:
```plaintext
Juan
```

## Profundizando
JSON, acrónimo de JavaScript Object Notation, ha sido popular desde principios de los 2000s. Alternativas como XML son más verbosas y complejas para trabajar en algunos contextos. Al trabajar con JSON en PowerShell, la versión 6.0+ mejora el soporte con los cmdlets `ConvertFrom-Json` y `ConvertTo-Json`, ofreciendo mejor manejo de profundidad y compatibilidad.

## Ver También
- [JSON Structured Data](https://json.org/)
- [PowerShell Gallery: PSGalleryModule](https://www.powershellgallery.com/packages/PSGalleryModule)
- [Introducción a PowerShell en Microsoft Docs](https://docs.microsoft.com/es-es/powershell/scripting/overview?view=powershell-7.1)
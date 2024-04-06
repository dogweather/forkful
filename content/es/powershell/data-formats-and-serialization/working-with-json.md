---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:21.299917-07:00
description: "C\xF3mo hacerlo: Para leer o analizar JSON en PowerShell, puedes usar\
  \ el cmdlet `ConvertFrom-Json`. Dada una cadena JSON, este cmdlet la convierte en\
  \ un\u2026"
lastmod: '2024-03-13T22:44:59.313778-06:00'
model: gpt-4-0125-preview
summary: Para leer o analizar JSON en PowerShell, puedes usar el cmdlet `ConvertFrom-Json`.
title: Trabajando con JSON
weight: 38
---

## Cómo hacerlo:


### Analizando JSON
Para leer o analizar JSON en PowerShell, puedes usar el cmdlet `ConvertFrom-Json`. Dada una cadena JSON, este cmdlet la convierte en un objeto de PowerShell.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

Salida de muestra:

```
John Doe
```

Este ejemplo demuestra cómo analizar una cadena JSON simple para acceder a las propiedades del objeto resultante.

### Generando JSON
Para generar JSON a partir de un objeto PowerShell, puedes usar el cmdlet `ConvertTo-Json`. Esto es útil para preparar datos para ser enviados a un servicio web o guardados en un archivo de configuración.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Ángeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

Salida de muestra:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Ángeles"
}
```

Este fragmento de código crea un objeto PowerShell y luego lo convierte a una cadena JSON.

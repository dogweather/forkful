---
date: 2024-01-20 17:33:31.228950-07:00
description: "C\xF3mo: Aqu\xED est\xE1 el c\xF3mo. Digamos que tienes dos fechas:\
  \ `$fecha1` y `$fecha2`. Quieres compararlas. Usa `-lt` para \"menor que\", `-gt`\
  \ para \"mayor que\" y\u2026"
lastmod: '2024-04-05T22:38:59.703464-06:00'
model: gpt-4-1106-preview
summary: "Aqu\xED est\xE1 el c\xF3mo. Digamos que tienes dos fechas: `$fecha1` y `$fecha2`.\
  \ Quieres compararlas. Usa `-lt` para \"menor que\", `-gt` para \"mayor que\" y\
  \ `-eq` para \"igual que\". As\xED se ve en acci\xF3n."
title: "Comparaci\xF3n de dos fechas"
weight: 27
---

## Cómo:
Aquí está el cómo. Digamos que tienes dos fechas: `$fecha1` y `$fecha2`. Quieres compararlas. Usa `-lt` para "menor que", `-gt` para "mayor que" y `-eq` para "igual que". Así se ve en acción:

```PowerShell
$fecha1 = Get-Date "2023-01-01"
$fecha2 = Get-Date "2023-12-31"

# ¿Es la fecha1 menor que la fecha2?
$fecha1 -lt $fecha2  # Retorna True

# ¿Es la fecha1 mayor que la fecha2?
$fecha1 -gt $fecha2  # Retorna False

# ¿Son la fecha1 y fecha2 iguales?
$fecha1 -eq $fecha2  # Retorna False
```

Simple y directo, ¿no?

## Deep Dive
Allá por los 80, comparar fechas era más complicado. Hoy, PowerShell lo simplifica bastante. Pero hay alternativas. Por ejemplo, puedes usar el método `CompareTo` o las funciones `Before` y `After` de objetos de tipo `[DateTime]`. Hablando de implementación, PowerShell trata a las fechas como objetos `[DateTime]`, lo cual es útil porque vienen con métodos propios.

Detrás de las cortinas, cuando comparas fechas, PowerShell transforma las fechas en números que representan ticks (unidad más pequeña de tiempo en .NET, igual a 100 nanosegundos) y luego los compara. Esto es más eficiente y preciso que trabajar con segundos o minutos.

Si quieren ir más allá, prueben con `[timespan]` para obtener la diferencia entre dos fechas, o jueguen con la zona horaria si trabajan en un contexto global.

## See Also
Para más detalles, estos recursos te pueden ayudar:

- La documentación de Microsoft sobre [datetime] y [timespan](https://docs.microsoft.com/en-us/dotnet/api/system.datetime).
- Página sobre operadores de comparación en PowerShell: [about_Comparison_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators).

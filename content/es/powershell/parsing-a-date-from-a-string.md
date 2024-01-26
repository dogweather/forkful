---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:38:07.121054-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Parsear una fecha desde un texto significa convertir texto en un objeto de fecha que PowerShell entiende. Los programadores hacen esto para manejar fechas correctamente: compararlas, modificarlas, o simplemente para tener un formato uniforme.

## Cómo hacerlo:
```PowerShell
# Parsear una fecha de texto simple
$fechaTexto = '25/03/2023'
$fecha = [DateTime]::ParseExact($fechaTexto, 'dd/MM/yyyy', $null)
echo $fecha

# Manejar formatos diferentes con cultura específica (español de España por ejemplo)
$fechaTextoEsp = '25-Marzo-2023'
$culturaEsp = [Globalization.CultureInfo]::CreateSpecificCulture('es-ES')
$fechaEsp = [DateTime]::ParseExact($fechaTextoEsp, 'dd-MMMM-yyyy', $culturaEsp)
echo $fechaEsp

# Si tienes un formato ISO 8601 puedes usar otra forma simplificada
$fechaISO = '2023-03-25T14:45:00'
$fecha = [datetime]::Parse($fechaISO, [Globalization.CultureInfo]::InvariantCulture, [Globalization.DateTimeStyles]::RoundtripKind)
echo $fecha
```

## Inmersión Profunda:
El parseo de fechas existe porque cada sistema, región o programa podría mostrar fechas en distintos formatos. Desde los días de .NET Framework, PowerShell ha usado las clases de fecha y hora de .NET para parsear y manejar fechas.

Además del método `ParseExact`, puedes usar `TryParse` y `TryParseExact` para manejar errores si el formato no es correcto, evitando que tu script falle.

Implementando la cultura de la forma correcta es crucial para que los scripts funcionen en entornos multiculturales. Esto significa que, al dar formato a una fecha, debes asegurarte de que PowerShell interprete el mes y día correctamente asociándole una cultura específica.

## Ver También:
- Microsoft Docs sobre [DateTime] en PowerShell: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- Información sobre culturas (CultureInfo) en .NET: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo
- Explicación de las variantes de fecha y hora ISO 8601: https://en.wikipedia.org/wiki/ISO_8601

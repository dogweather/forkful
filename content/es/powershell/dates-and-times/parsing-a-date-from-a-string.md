---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:00.886331-07:00
description: "Analizar una fecha de una cadena se trata de reconocer y convertir fechas\
  \ escritas en forma de texto a un tipo de datos de fecha que PowerShell pueda\u2026"
lastmod: '2024-03-13T22:44:59.301518-06:00'
model: gpt-4-0125-preview
summary: Analizar una fecha de una cadena se trata de reconocer y convertir fechas
  escritas en forma de texto a un tipo de datos de fecha que PowerShell pueda entender
  y con el que pueda trabajar.
title: Analizando una fecha a partir de una cadena de texto
weight: 30
---

## Qué & Por qué?
Analizar una fecha de una cadena se trata de reconocer y convertir fechas escritas en forma de texto a un tipo de datos de fecha que PowerShell pueda entender y con el que pueda trabajar. Los programadores hacen esto para manipular, formatear, comparar o calcular fechas, tareas comunes en scripts que manejan archivos de registro, entrada de usuarios o procesamiento de datos.

## Cómo hacerlo:
PowerShell hace que el análisis de fechas desde cadenas sea directo con su cmdlet `Get-Date` y el acelerador de tipo `[datetime]`, que funcionan bien para formatos de fecha estándar. Para cadenas de fecha más complejas o no estándar, se puede utilizar el método `[datetime]::ParseExact` para especificar el formato exacto.

### Utilizando `Get-Date` y `[datetime]`:
```powershell
# Conversión simple usando Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Ejemplo de salida:**
```
Saturday, April 1, 2023 12:00:00 AM
```

```powershell
# Usando el acelerador de tipo [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Ejemplo de salida:**
```
Saturday, April 1, 2023 12:00:00 AM
```

### Utilizando `[datetime]::ParseExact` para formatos no estándar:
Para formatos no reconocidos automáticamente, puedes definir el formato exacto para garantizar un análisis correcto.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Ejemplo de salida:**
```
Saturday, April 1, 2023 2:00:00 PM
```

### Aprovechando Bibliotecas de Terceros
Aunque PowerShell es bastante potente para el análisis de fechas, para escenarios muy complejos o funcionalidades adicionales, podrías explorar bibliotecas de .NET como NodaTime, aunque para muchos casos de uso típicos, las capacidades nativas de PowerShell serán suficientes.

```powershell
# Usando NodaTime solo como ilustración, nota que necesitas agregar la biblioteca a tu proyecto
# Install-Package NodaTime -Version 3.0.5
# Usando NodaTime para analizar una fecha
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Nota Importante:** El código anterior es una ilustración conceptual. En la práctica, asegúrate de que NodaTime esté correctamente agregado a tu proyecto para que los tipos y métodos estén disponibles.

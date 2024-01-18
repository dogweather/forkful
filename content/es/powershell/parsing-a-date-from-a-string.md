---
title:                "Analizando una fecha de una cadena"
html_title:           "PowerShell: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¡Qué es y por qué hacerlo?
La manipulación de fechas es una tarea común en la programación. Cuando se trabaja con fechas en formato de texto, es necesario convertirlas a un formato que el ordenador pueda entender y manipular. Esto se conoce como analizar una fecha desde una cadena de texto. Los programadores hacen esto para poder realizar cálculos y comparaciones con fechas de manera más fácil y precisa.

## Cómo:
```PowerShell
# Ejemplo básico: convirtiendo una cadena de texto a un objeto de fecha y hora.
$fecha = "01/01/2021"
[DateTime]$fechaObjeto = [DateTime]::Parse($fecha)
```
El código anterior convierte la cadena de texto "01/01/2021" a un objeto de fecha y hora en el formato predeterminado del sistema operativo. Dependiendo del formato de la fecha en la cadena de texto, puede ser necesario especificar un formato de fecha personalizado en la función `Parse()`. 
```PowerShell
# Ejemplo avanzado: manipulando la fecha y hora.
$fecha = "01/01/2021 10:00 AM"
[DateTime]$fechaObjeto = [DateTime]::ParseExact($fecha, "dd/MM/yyyy hh:mm tt", $null)
# Imprimir solo la fecha.
$fechaObjeto.Date
# Salida: 01/01/2021
# Agregando días a la fecha.
$fechaObjeto.AddDays(7)
# Salida: 08/01/2021 10:00 AM
```

## Inmersión profunda:
Analizar fechas de cadenas de texto ha sido una tarea compleja en los primeros lenguajes de programación. Sin embargo, con la introducción de .NET Framework, se ha vuelto mucho más fácil y preciso gracias a la clase `DateTime` y sus métodos `Parse()` y `ParseExact()`. Alternativas a la hora de manipular fechas incluyen el uso de bibliotecas externas o el uso de expresiones regulares para extraer los distintos componentes de una fecha en una cadena de texto. 

## Ver también:
- [Documentación oficial de Microsoft sobre la clase DateTime](https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0)
- [Método Parse de la clase DateTime](https://docs.microsoft.com/es-es/dotnet/api/system.datetime.parse?view=net-5.0)
- [Método ParseExact de la clase DateTime](https://docs.microsoft.com/es-es/dotnet/api/system.datetime.parseexact?view=net-5.0)
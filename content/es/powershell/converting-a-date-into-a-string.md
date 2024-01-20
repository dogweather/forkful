---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Convertir una fecha en una cadena en PowerShell implica transformar una variable de tipo `DateTime` en un `string` para su manipulación o presentación. Los programadores lo hacen para hacer complejas operaciones de fecha y hora más manejables y para presentar la información en un formato legible para el usuario.

## ¿Cómo hacerlo?

El siguiente muestra cómo convertir una fecha en una cadena en PowerShell:

```PowerShell
$Fecha = Get-Date
$Cadena = $Fecha.ToString('yyyy-MM-dd')
$Cadena
```

En esto, `$Fecha` recibe la fecha y hora actuales. `$Fecha.ToString('yyyy-MM-dd')` convierte la fecha en una cadena en el formato "año-mes-día", que se asigna a `$Cadena`. La instrucción final imprime la cadena.

El resultado es similarmente:

```
2021-12-26
```
## Inmersión profunda

Para entender esto más a fondo, observe que la función `ToString()` es extremadamente flexible, capaz de adoptar muchos formatos diferentes. 

Históricamente, esta flexibilidad proviene porque .NET, desde su lanzamiento en 2002, siempre ha manejado fechas y horas con el mismo tipo de objeto (`DateTime`). PowerShell, basado en .NET, hereda esto y puede acceder a todos sus métodos.

Este proceso tiene alternativas. Por ejemplo, puede usar el comando `Cmdlet` de PowerShell `Format-Date`:

```PowerShell
$Fecha = Get-Date
$Cadena = $Fecha | Format-Date -Format 'yyyy-MM-dd'
$Cadena
```

Este código hace esencialmente lo mismo que el anterior, pero utiliza tuberías y `Cmdlet`, que es más habitual en PowerShell.

Acerca los detalles de la implementación, `ToString()` no sólo convierte objetos en cadenas, también formatea estos objetos. Puede elegir entre una gran cantidad de códigos predefinidos para personalizar la cadena de salida para que se ajuste a sus necesidades. 

Por ejemplo, `ToString('MM/dd/yyyy')` dará la fecha en formato "mes/día/año”, mientras que `ToString('dddd')` nos mostrará el nombre del día de la semana.

## Ver también

- [Formato de cadena personalizado para DateTime](https://docs.microsoft.com/es-es/dotnet/standard/base-types/custom-date-and-time-format-strings?view=netframework-4.7.2)
- [Clase DateTime](https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0)
- [Get-Date (Cmdlet de PowerShell)](https://docs.microsoft.com/es-es/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)
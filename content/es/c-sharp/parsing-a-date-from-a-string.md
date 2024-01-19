---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Interpretando una fecha desde una cadena en C#

## ¿Qué & Por qué?
Interpretar una fecha desde una cadena es el proceso de convertir un `string` que representa una fecha y/o hora en un objeto `DateTime`. Los programadores a menudo hacen esto para tratar con fechas y horas en formatos de texto, como entradas de usuario o datos en bruto de un archivo.

## Cómo hacerlo:

Aquí hay un ejemplo básico de cómo hacer esto en C#.

```C#
string fechaCadena = "13/10/2021";
DateTime fecha;
if (DateTime.TryParse(fechaCadena, out fecha))
{
    Console.WriteLine($"La fecha es {fecha}.");
}
else
{
    Console.WriteLine("No se pudo interpretar la fecha.");
}
```
Output

```C#
La fecha es 13/10/2021 00:00:00.
```
Por favor, tenga en cuenta que, dependiendo de la configuración regional de su sistema, es posible que necesite ajustar el formato de fecha en la cadena.

## Inmersión Profunda:

### 1. Contexto histórico:
El soporte para interpretar fechas desde una cadena se ha incluido en C# desde su primer lanzamiento en 2002. El método `DateTime.TryParse()` se introdujo en .NET Framework 2.0 para facilitar la interpretación segura de las fechas.

### 2. Alternativas:
Si sabe exactamente en qué formato vendrá la cadena, puede usar `DateTime.ParseExact()` o `DateTime.TryParseExact()`, que ofrecen un mayor control sobre el formato de fecha y hora.

```C#
string fechaExacta = "13 Oct 2021 18:30";
string formato = "dd MMM yyyy HH:mm";
CultureInfo proveedor = CultureInfo.InvariantCulture;
DateTime fecha;
if (DateTime.TryParseExact(fechaExacta, formato, proveedor, DateTimeStyles.None, out fecha))
{
    Console.WriteLine($"La fecha es: {fecha}.");
}
else
{
    Console.WriteLine("No se pudo interpretar la fecha.");
}
```
Output

```C#
La fecha es: 13/10/2021 18:30:00
```
### 3. Detalles de implementación:
El procesamiento de la cadena de fecha se realiza internamente mediante la clase `DateTimeFormat`, que utiliza la configuración regional del sistema para determinar el formato de fecha correcto, a menos que se especifique un proveedor de cultura.

## Ver También:

1. Referencia de Microsoft a `DateTime.TryParse` - [enlace](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse)

2. Referencia de Microsoft a `DateTime.ParseExact` - [enlace](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact)

3. Tutorial sobre `DateTime` en C# - [enlace](https://www.c-sharpcorner.com/blogs/date-and-time-format-in-c-sharp-programming1)

4. Guía para trabajar con fechas y horas en C# - [enlace](https://docs.microsoft.com/en-us/dotnet/standard/datetime)
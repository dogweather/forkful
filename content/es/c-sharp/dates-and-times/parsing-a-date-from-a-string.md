---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:52.574322-07:00
description: "C\xF3mo hacerlo: **An\xE1lisis B\xE1sico:** Los m\xE9todos `DateTime.Parse`\
  \ y `DateTime.TryParse` son las opciones predilectas para convertir una cadena en\u2026"
lastmod: '2024-04-05T21:54:00.430457-06:00'
model: gpt-4-0125-preview
summary: "**An\xE1lisis B\xE1sico:** Los m\xE9todos `DateTime.Parse` y `DateTime.TryParse`\
  \ son las opciones predilectas para convertir una cadena en `DateTime`."
title: Analizando una fecha a partir de una cadena de texto
weight: 30
---

## Cómo hacerlo:
**Análisis Básico:**

Los métodos `DateTime.Parse` y `DateTime.TryParse` son las opciones predilectas para convertir una cadena en `DateTime`. Aquí hay un ejemplo rápido:

```csharp
string dateString = "2023-04-12";
DateTime fechaAnalizada;

if (DateTime.TryParse(dateString, out fechaAnalizada))
{
    Console.WriteLine($"Analizado con éxito: {fechaAnalizada}");
}
else
{
    Console.WriteLine("Fallo al analizar.");
}
// Salida: Analizado con éxito: 4/12/2023 12:00:00 a. m.
```

**Especificar una Cultura:**

A veces, necesitas analizar una cadena de fecha que está en un formato de cultura específico. Puedes lograrlo usando la clase `CultureInfo`:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime fechaAnalizada = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(fechaAnalizada);
// Salida: 4/12/2023 12:00:00 a. m.
```

**Análisis Exacto con un Formato Específico:**

Para escenarios donde las fechas vienen en un formato específico que podría no ser estándar, `DateTime.ParseExact` es muy útil:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime fechaAnalizada = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(fechaAnalizada);
// Salida: 4/12/2023 12:00:00 a. m.
```

**Usando NodaTime:**

Para un análisis de fecha y hora aún más robusto, considera usar la popular biblioteca de terceros NodaTime. Proporciona un rango más amplio de capacidades de manejo de fechas y horas:

```csharp
using NodaTime;
using NodaTime.Text;

var patrón = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var resultadoDelAnálisis = patrón.Parse("2023-04-12");

if (resultadoDelAnálisis.Success)
{
    LocalDate fechaLocal = resultadoDelAnálisis.Value;
    Console.WriteLine(fechaLocal); // 2023-04-12
}
else
{
    Console.WriteLine("Fallo al analizar.");
}
```

NodaTime ofrece un amplio apoyo para zonas horarias, conceptos de periodo y duración, y muchos sistemas calendáricos diferentes, lo que lo convierte en una elección poderosa para la manipulación compleja de fechas y horas en aplicaciones .NET.

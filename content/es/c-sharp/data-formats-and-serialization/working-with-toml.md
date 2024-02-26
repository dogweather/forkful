---
date: 2024-01-26 04:20:02.511376-07:00
description: "TOML es un acr\xF3nimo de Tom's Obvious, Minimal Language, un formato\
  \ de archivo de configuraci\xF3n que es f\xE1cil de leer debido a sus sem\xE1nticas\
  \ claras. Los\u2026"
lastmod: '2024-02-25T18:49:55.571273-07:00'
model: gpt-4-0125-preview
summary: "TOML es un acr\xF3nimo de Tom's Obvious, Minimal Language, un formato de\
  \ archivo de configuraci\xF3n que es f\xE1cil de leer debido a sus sem\xE1nticas\
  \ claras. Los\u2026"
title: Trabajando con TOML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
TOML es un acrónimo de Tom's Obvious, Minimal Language, un formato de archivo de configuración que es fácil de leer debido a sus semánticas claras. Los programadores lo usan para archivos de configuración, simplificando el intercambio de datos entre sistemas, y porque logra un equilibrio entre la legibilidad humana y la capacidad de análisis por máquinas.

## Cómo hacerlo:
Primero, instala un analizador de TOML como `Tomlyn`. Usa tu gestor de paquetes:

```csharp
dotnet add package Tomlyn
```

Luego, analiza un archivo TOML:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Propietario: {tomlTable["owner"]["name"]}");
// Salida:
// Propietario: Tom Preston-Werner
```

Ahora, crea y escribe TOML:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML escrito en config.toml");
// Salida:
// TOML escrito en config.toml
```

## Inmersión Profunda:
TOML fue creado por Tom Preston-Werner, cofundador de GitHub, alrededor de 2013 como reacción a las limitaciones de formatos existentes como YAML y JSON en configuraciones. Está específicamente diseñado para configuraciones con un fuerte énfasis en ser directo y sin ambigüedades.

Entre los formatos de configuración alternativos se incluyen YAML, JSON y XML. Sin embargo, TOML destaca por ser más amigable para los humanos, particularmente para archivos de configuración donde la edición manual es común. JSON, aunque ubicuo, es menos legible para configuraciones complejas, y XML es verboso. YAML, aunque similar en legibilidad, puede complicarse con el uso intensivo de espacios en blanco y presenta riesgos de seguridad con cierto contenido.

En términos de implementación, TOML se enfoca en mapear de manera limpia a una tabla hash, haciendo que la extracción de datos sea predecible. Con el lanzamiento de la versión 1.0.0, TOML solidificó su especificación, mejorando la estabilidad y el soporte de herramientas.

## Vea También:
- Repositorio y Especificación Oficiales de TOML en GitHub: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, la biblioteca de .NET: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)

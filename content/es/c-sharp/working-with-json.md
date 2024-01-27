---
title:                "Trabajando con JSON"
date:                  2024-01-19
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
JSON (JavaScript Object Notation) es un formato ligero de intercambio de datos. Los programadores lo usamos porque es fácil de leer y escribir para humanos, y fácil de parsear y generar para máquinas.

## Cómo hacerlo:
Para trabajar con JSON en C#, usualmente usamos `Json.NET`, una biblioteca popular que facilita serializar y deserializar datos. Vamos a ver cómo usarla.

```C#
using Newtonsoft.Json;
using System;

public class Program
{
    public static void Main()
    {
        // Serialización: Convertir un objeto a una cadena JSON
        Usuario usuario = new Usuario() { Nombre = "Juan", Edad = 30 };
        string json = JsonConvert.SerializeObject(usuario);
        Console.WriteLine(json);

        // Deserialización: Convertir una cadena JSON a un objeto
        string jsonInput = @"{ 'Nombre': 'Ana', 'Edad': 25 }";
        Usuario usuarioDeserializado = JsonConvert.DeserializeObject<Usuario>(jsonInput);
        Console.WriteLine($"Nombre: {usuarioDeserializado.Nombre}, Edad: {usuarioDeserializado.Edad}");
    }
}

public class Usuario
{
    public string Nombre { get; set; }
    public int Edad { get; set; }
}
```

La salida será:
```
{"Nombre":"Juan","Edad":30}
Nombre: Ana, Edad: 25
```

## Profundizando
JSON apareció en los 2000, creado por Douglas Crockford como una alternativa a XML para la representación de datos. A diferencia de XML, JSON es mucho más simple y ligero. Las principales alternativas hoy en día incluyen YAML, pero JSON sigue siendo el preferido en APIs web y configuraciones por su compatibilidad y velocidad. En C# .NET Core y .NET 5+, System.Text.Json es otra opción para trabajar con JSON, ofreciendo un rendimiento mejorado y menor uso de memoria.

## Ver También
- Documentación oficial de Json.NET: https://www.newtonsoft.com/json
- Guía de Microsoft sobre System.Text.Json: https://docs.microsoft.com/dotnet/standard/serialization/system-text-json-overview
- Comparativa entre Json.NET y System.Text.Json: https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-migrate-from-newtonsoft-how-to

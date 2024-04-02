---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:58.110505-07:00
description: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un formato de serializaci\xF3n de datos legible por humanos. Los\u2026"
lastmod: '2024-03-13T22:44:59.097927-06:00'
model: gpt-4-0125-preview
summary: "YAML, que significa \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un formato de serializaci\xF3n de datos legible por humanos. Los\u2026"
title: Trabajando con YAML
weight: 41
---

## ¿Qué y Por Qué?
YAML, que significa "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), es un formato de serialización de datos legible por humanos. Los programadores a menudo lo utilizan para archivos de configuración, mensajería entre procesos y almacenamiento de datos debido a su simplicidad y legibilidad en comparación con otros formatos de datos como XML o JSON.

## Cómo hacerlo:
C# no tiene soporte incorporado para YAML, pero puedes trabajar fácilmente con YAML utilizando bibliotecas de terceros como *YamlDotNet*. Primero, necesitas instalar el paquete YamlDotNet:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### Leyendo YAML:
Imagina que tienes un archivo YAML `config.yaml` con el siguiente contenido:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

Puedes leer y analizar este archivo YAML en C# de la siguiente manera:
```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class AppConfig
{
    public AppSettings appSettings { get; set; }
}

public class AppSettings
{
    public string name { get; set; }
    public string version { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var yaml = File.ReadAllText("config.yaml");
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Ajusta la convención de nombres según sea necesario
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Nombre: {config.appSettings.name}, Versión: {config.appSettings.version}");
    }
}
```
**Salida de muestra:**
```
Nombre: MyApp, Versión: 1.0.0
```

### Escribiendo YAML:
Para escribir datos en un archivo YAML, usa la clase `Serializer` de YamlDotNet. Así es como serializas un objeto de vuelta a YAML:

```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program
{
    static void Main(string[] args)
    {
        var config = new AppConfig
        {
            appSettings = new AppSettings
            {
                name = "MyApp",
                version = "2.0.0"
            }
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // Ajusta la convención de nombres según sea necesario
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**Salida de muestra:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

Este enfoque directo demuestra cómo trabajar eficientemente con YAML en tus proyectos de C#, facilitando la lectura y escritura en archivos YAML utilizando la biblioteca YamlDotNet.

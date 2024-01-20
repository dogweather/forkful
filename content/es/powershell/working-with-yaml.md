---
title:                "Trabajando con yaml"
html_title:           "PowerShell: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y para qué?
Trabajar con YAML es una forma de organizar y estructurar datos de forma sencilla y legible para humanos y computadoras. Los programadores usan YAML para guardar información en un formato fácil de interpretar y modificar, lo que lo hace útil para configuraciones de software y archivos de datos.

## Cómo hacerlo:
Aquí hay un ejemplo de código en PowerShell para crear un archivo YAML:

```PowerShell
New-YamlFile -Path C:\Prueba\config.yml -Value @{
    GoogleAnalytics = @{
        TrackingID = "UA-123456-01"
        IncludeIP = $true
    }
    Database = @{
        Username = "usuario"
        Password = "contraseña"
        Name = "mi_base_de_datos"
    }
}
```

Este código generará un archivo YAML llamado "config.yml" con los valores especificados. Aquí está un ejemplo de cómo podría verse el archivo:

```
GoogleAnalytics:
  TrackingID: UA-123456-01
  IncludeIP: true
Database:
  Username: usuario
  Password: contraseña
  Name: mi_base_de_datos
```

Para leer un archivo YAML existente y obtener los valores almacenados, puedes usar este código:

```PowerShell
$file = Get-YamlFile -Path C:\Prueba\config.yml
$file.GoogleAnalytics.TrackingID # Imprime "UA-123456-01"
```

## Inmersión profunda:
YAML fue creado en 2001 y su principal objetivo es ser un formato de serialización de datos legible por humanos. Aunque YAML es compatible con muchos lenguajes de programación, se ha vuelto especialmente popular en la comunidad de DevOps como una forma de configurar y organizar la infraestructura de software.

Si no tienes instalado el módulo de PowerShell para trabajar con YAML, puedes usar otras herramientas como el comando `ConvertTo-Yaml` o algún editor de texto con soporte para YAML.

Para aprender más sobre YAML, puedes consultar la documentación oficial o revisar algunos proyectos de código abierto que utilizan YAML para sus configuraciones.

## Ver también:
- [YAML.org](https://yaml.org/)
---
title:                "Trabajando con TOML"
date:                  2024-01-26T04:24:40.901205-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con TOML"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

TOML, abreviatura de Tom's Obvious, Minimal Language (Lenguaje Mínimo y Obvio de Tom), es un formato de serialización de datos que es fácil de leer debido a su semántica clara. Los programadores lo utilizan para archivos de configuración, ya que encuentra un equilibrio entre ser legible para humanos y amigable para máquinas.

## Cómo hacerlo:

En PowerShell, no hay un cmdlet nativo para analizar TOML. Típicamente usarías un módulo o convertirías TOML a JSON con una herramienta como `toml-to-json` si quieres trabajar con PowerShell. Así es como lo harías con un módulo ficticio `PowerShellTOML`:

```PowerShell
# Primero, instala el módulo (imaginario, para demostración)
Install-Module PowerShellTOML

# Importa un archivo TOML
$config = Import-TomlConfig -Path './config.toml'

# Accediendo a un valor
Write-Output $config.database.server

# Contenido TOML de muestra en 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Salida de muestra:
# 192.168.1.1
```

## Profundización

TOML fue creado por Tom Preston-Werner, cofundador de GitHub, como una alternativa más simple a XML y YAML para archivos de configuración. Su primera versión apareció en 2013. TOML es comparable a JSON, pero está diseñado para ser más amigable para humanos, por lo que es una buena opción para configuraciones mantenidas por personas. Las alternativas incluyen YAML, JSON y XML.

En términos de implementación, un módulo de PowerShell para TOML típicamente sería un envoltorio alrededor de una biblioteca TOML escrita en un lenguaje más orientado al rendimiento como C#. PowerShell no tiene soporte incorporado para TOML, razón por la cual tal módulo es necesario para interactuar cómodamente con el formato TOML.

## Ver También

- Estándar de TOML: https://toml.io/en/
- Repositorio de GitHub para el módulo `toml` de PowerShell (si existe en el momento de la lectura): https://github.com/powershell/PowerShellTOML
- Una introducción a TOML: https://github.com/toml-lang/toml
- Comparación de formatos de serialización de datos: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats

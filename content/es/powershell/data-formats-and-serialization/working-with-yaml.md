---
title:                "Trabajando con YAML"
aliases:
- es/powershell/working-with-yaml.md
date:                  2024-02-03T19:26:12.974723-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
YAML, o YAML Ain't Markup Language (YAML No Es Un Lenguaje de Marcado), es un lenguaje de serialización de datos legible por humanos. Los programadores a menudo lo utilizan para archivos de configuración y transmisión de datos entre lenguajes. Su simplicidad y legibilidad lo hacen particularmente popular para tareas que involucran la configuración de entornos, aplicaciones o servicios donde las configuraciones son cruciales y deben ser fácilmente entendidas y editadas.

## Cómo:
PowerShell, por defecto, no viene con un cmdlet integrado para analizar YAML, pero funciona sin problemas con YAML cuando aprovechas el módulo `powershell-yaml` o conviertes YAML en un objeto de PowerShell usando `ConvertFrom-Json` en combinación con una herramienta como `yq`.

### Usando el Módulo `powershell-yaml`:
Primero, instala el módulo:
```PowerShell
Install-Module -Name powershell-yaml
```

Para leer un archivo YAML:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

Para escribir un objeto de PowerShell en un archivo YAML:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Ejemplo de `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Analizando YAML con `yq` y `ConvertFrom-Json`:
Otro enfoque implica usar `yq`, un procesador de YAML de línea de comandos ligero y portátil. `yq` puede convertir YAML en JSON, el cual PowerShell puede analizar de manera nativa.

Primero, asegúrate de que `yq` esté instalado en tu sistema.
Luego ejecuta:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Este método es particularmente útil para usuarios que trabajan en entornos multiplataforma o prefieren usar JSON dentro de PowerShell.

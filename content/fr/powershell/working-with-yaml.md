---
title:                "Travailler avec YAML"
date:                  2024-01-19
simple_title:         "Travailler avec YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, c'est quoi ? C'est un format de données lisible par l'humain, utilisé pour la configuration des applications. Pourquoi on s'en sert ? YAML est souvent préféré pour sa facilité de lecture et d'écriture comparé à XML ou JSON.

## How to:

Pour travailler avec YAML en PowerShell, il faut souvent passer par un module comme `powershell-yaml`. On l'installe, on lit, et on écrit du YAML.

```PowerShell
# Installer le module PowerShell-Yaml
Install-Module -Name powershell-yaml

# Lire un fichier YAML
$yamlContent = Get-Content -Path 'config.yaml' | ConvertFrom-Yaml
Write-Host "Contenu du YAML:" $yamlContent

# Modifier le contenu
$yamlContent['setting'] = "newValue"

# Écrire dans un fichier YAML
$yamlContent | ConvertTo-Yaml | Set-Content -Path 'config.yaml'
Write-Host "YAML mis à jour."
```

## Deep Dive

YAML est né en 2001, conçu pour être plus simple que XML pour les humains à lire et à écrire. En PowerShell, `powershell-yaml` n'est pas la seule option : il y a aussi `YamlDotNet` et d'autres alternatives. Mais `powershell-yaml` est facile à utiliser et s'intègre bien dans l'écosystème PowerShell. Techniquement, YAML se base sur l'indentation pour refléter la structure de données, ce qui le rend sensible aux erreurs liées à l'espacement.

## See Also

- YAML Specification: https://yaml.org/spec/
- GitHub Page for PowerShell-Yaml Module: https://github.com/dfinke/PowerShellYaml
- YamlDotNet, une autre bibliothèque pour gérer le YAML en .NET: https://github.com/aaubry/YamlDotNet

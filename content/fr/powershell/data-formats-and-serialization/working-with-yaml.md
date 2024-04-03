---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:13.197468-07:00
description: "Comment faire : PowerShell, par d\xE9faut, ne vient pas avec un cmdlet\
  \ int\xE9gr\xE9 pour l'analyse de YAML, mais il fonctionne de mani\xE8re transparente\
  \ avec YAML\u2026"
lastmod: '2024-03-13T22:44:58.080645-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, par d\xE9faut, ne vient pas avec un cmdlet int\xE9gr\xE9 pour\
  \ l'analyse de YAML, mais il fonctionne de mani\xE8re transparente avec YAML lorsque\
  \ vous tirez parti du module `powershell-yaml` ou convertissez du YAML en un objet\
  \ PowerShell en utilisant `ConvertFrom-Json` en combinaison avec un outil comme\
  \ `yq`."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
PowerShell, par défaut, ne vient pas avec un cmdlet intégré pour l'analyse de YAML, mais il fonctionne de manière transparente avec YAML lorsque vous tirez parti du module `powershell-yaml` ou convertissez du YAML en un objet PowerShell en utilisant `ConvertFrom-Json` en combinaison avec un outil comme `yq`.

### Utilisation du module `powershell-yaml` :
D'abord, installez le module :
```PowerShell
Install-Module -Name powershell-yaml
```

Pour lire un fichier YAML :
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

Pour écrire un objet PowerShell dans un fichier YAML :
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Exemple de `output.yml` :
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Analyse de YAML avec `yq` et `ConvertFrom-Json` :
Une autre approche implique l'utilisation de `yq`, un processeur YAML en ligne de commande léger et portable. `yq` peut convertir du YAML en JSON, que PowerShell peut analyser nativement.

D'abord, assurez-vous que `yq` est installé sur votre système.
Ensuite, exécutez :
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Cette méthode est particulièrement utile pour les utilisateurs qui travaillent dans des environnements multi-plateformes ou préfèrent utiliser JSON au sein de PowerShell.

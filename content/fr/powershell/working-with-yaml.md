---
title:                "Travailler avec YAML"
date:                  2024-02-03T19:26:13.197468-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
YAML, ou "YAML Ain't Markup Language" (YAML n'est pas un langage de balisage), est un langage de sérialisation de données lisible par l'humain. Les programmeurs l'utilisent souvent pour les fichiers de configuration et la transmission de données entre langages. Sa simplicité et sa lisibilité le rendent particulièrement populaire pour des tâches impliquant la configuration d'environnements, d'applications ou de services où les configurations sont cruciales et doivent être facilement comprises et modifiées.

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

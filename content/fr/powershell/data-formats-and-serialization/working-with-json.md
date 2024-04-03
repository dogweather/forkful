---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:25.391991-07:00
description: 'Comment faire : #.'
lastmod: '2024-03-13T22:44:58.082935-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Travailler avec JSON
weight: 38
---

## Comment faire :


### Parser le JSON
Pour lire ou parser du JSON dans PowerShell, vous pouvez utiliser le cmdlet `ConvertFrom-Json`. Étant donné une chaîne JSON, ce cmdlet la convertit en un objet PowerShell.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

Exemple de sortie :

```
John Doe
```

Cet exemple démontre comment parser une simple chaîne JSON pour accéder aux propriétés de l'objet résultant.

### Générer du JSON
Pour générer du JSON à partir d'un objet PowerShell, vous pouvez utiliser le cmdlet `ConvertTo-Json`. Cela est pratique pour préparer des données à envoyer à un service web ou à sauvegarder dans un fichier de configuration.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

Exemple de sortie :

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

Ce morceau de code crée un objet PowerShell puis le convertit en chaîne JSON.

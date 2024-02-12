---
title:                "Travailler avec JSON"
aliases: - /fr/powershell/working-with-json.md
date:                  2024-02-03T19:23:25.391991-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L’intégration de PowerShell avec le JSON (JavaScript Object Notation) concerne le parsing (lecture) et la génération (écriture) de données JSON, un format courant pour l'échange de données sur le web. Les programmeurs travaillent avec le JSON pour interagir avec des API web, des fichiers de configuration, ou pour faciliter l'échange de données entre différents langages et plateformes en raison de sa nature légère et indépendante du langage.

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

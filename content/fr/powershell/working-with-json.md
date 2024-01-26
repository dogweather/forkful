---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? 
JSON, c'est du texte pour échanger des données. Les devs l'aiment pour sa simplicité et son interopérabilité.

## How to:
### Lire JSON:
```PowerShell
$json = Get-Content -Path 'data.json' | ConvertFrom-Json
$json
```
### Écrire JSON:
```PowerShell
$obj = @{name = 'Jean'; age = 30}
$obj | ConvertTo-Json | Set-Content -Path 'output.json'
```
### Filtrage:
```PowerShell
$json.users | Where-Object { $_.age -gt 25 }
```

## Deep Dive
JSON arrive dans les années 2000. XML était le choix avant. Mais JSON gagne avec sa légèreté et sa facilité. PowerShell gère JSON nativement. Il respecte son modèle d'objets lors de la conversion vers et depuis JSON, ce qui simplifie la manipulation des données.

## See Also
- [Doc officielle PowerShell](https://docs.microsoft.com/fr-fr/powershell/)
- [JSON sur W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Comparaison JSON vs XML](https://www.json.org/xml.html)

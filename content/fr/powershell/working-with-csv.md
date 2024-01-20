---
title:                "Manipulation des fichiers CSV"
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, c'est quoi ? Des fichiers textes pour stocker des données, chaque ligne est un enregistrement, les valeurs séparées par des virgules. Pourquoi ? Parce que c'est simple, universel, et facile à manipuler programmation.

## How to:
### Importer un CSV
```PowerShell
# Charger un CSV dans une variable
$data = Import-Csv -Path 'chemin/vers/ton/fichier.csv'
```

### Afficher les données
```PowerShell
# Montrer le contenu de $data
$data
```

### Exporter en CSV
```PowerShell
# Exporter des données en CSV
$data | Export-Csv -Path 'chemin/vers/nouveau_fichier.csv' -NoTypeInformation
```

### Filtrer et sélectionner des données
```PowerShell
# Sélectionner des enregistrements avec filtrage
$dataFiltrés = $data | Where-Object { $_.age -gt 20 } | Select-Object nom, email
```

## Deep Dive
CSV (Comma-Separated Values) existe depuis les premiers jours de l'informatique personnelle ; tout outil qui traite des données peut travailler avec. Alternatives ? XML, JSON, bases de données—plus structurés, mais plus complexes. Implémentation ? PowerShell rend les CSV encore plus accessibles grâce à ses cmdlets intégrées comme `Import-Csv` et `Export-Csv`, conçus pour simplifier la manipulation de ces fichiers.

## See Also
- [Documentation officielle de `Import-Csv`](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/import-csv)
- [Documentation officielle de `Export-Csv`](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/export-csv)
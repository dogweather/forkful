---
title:                "Travailler avec les fichiers csv"
html_title:           "PowerShell: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Le CSV (Comma-Separated Values) est un format de fichier couramment utilisé pour stocker des données tabulaires telles que des feuilles de calcul. Les programmeurs travaillent avec des fichiers CSV car ils sont faciles à lire, à écrire et à traiter en utilisant des langages de programmation.

## Comment faire:

```PowerShell
# Pour importer des données à partir d'un fichier CSV:
$donnees = Import-Csv data.csv

# Pour exporter des données vers un fichier CSV:
$donnees | Export-Csv -Path output.csv

# Pour manipuler des données dans un fichier CSV:
$donnees | Where-Object {$_.colonne -eq "valeur"} | Select-Object colonne1, colonne2
```

## Plongée en profondeur:

Il est intéressant de noter que le format CSV a été créé dans les années 70 pour faciliter l'échange de données entre les systèmes informatiques. Bien que largement utilisé, il existe d'autres alternatives telles que JSON et XML pour stocker des données tabulaires.

En travaillant avec des fichiers CSV en PowerShell, il est important de noter que les valeurs sont généralement séparées par des virgules, mais il est possible de spécifier un autre caractère de séparation. De plus, l'utilisation de guillemets autour des valeurs est facultative, mais peut être nécessaire en cas d'utilisation de caractères spéciaux.

## À voir également:

Pour en savoir plus sur le format CSV et comment travailler avec en PowerShell, vous pouvez consulter les ressources suivantes:

- [Documentation Microsoft sur Import-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv?view=powershell-7.1)
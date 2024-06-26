---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:47.149600-07:00
description: "Comment faire : Pour lire un fichier CSV, utilisez l'applet de commande\
  \ `Import-Csv`. Cet applet de commande lit le fichier et le convertit en objets\u2026"
lastmod: '2024-03-13T22:44:58.084344-06:00'
model: gpt-4-0125-preview
summary: Pour lire un fichier CSV, utilisez l'applet de commande `Import-Csv`.
title: Travailler avec CSV
weight: 37
---

## Comment faire :


### Lire un fichier CSV
Pour lire un fichier CSV, utilisez l'applet de commande `Import-Csv`. Cet applet de commande lit le fichier et le convertit en objets personnalisés de PowerShell pour chaque ligne.

```powershell
# Importation d'un fichier CSV
$data = Import-Csv -Path "C:\Data\users.csv"
# Affichage du contenu
$data
```

**Exemple de sortie :**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### Écrire dans un fichier CSV
Inversement, pour écrire des données dans un fichier CSV, l'applet de commande `Export-Csv` est utilisé. Cet applet de commande prend les objets en entrée et les convertit en format CSV.

```powershell
# Création d'un objet à exporter
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# Exportation dans un fichier CSV
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Après exécution, un fichier nommé `new_users.csv` est créé avec les données fournies.

### Filtrer et Manipuler le Contenu d'un CSV
Pour filtrer ou manipuler les données d'un fichier CSV, utilisez les capacités de manipulation d'objets de PowerShell. Par exemple, pour sélectionner uniquement les utilisateurs au-dessus d'un certain âge et d'une ville spécifique :

```powershell
# Importation et filtrage des données
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# Afficher les données filtrées
$filteredData
```

**Exemple de sortie :**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### Utiliser des Bibliothèques Tierces
Bien que les cmdlets natifs de PowerShell soient généralement suffisants pour les tâches courantes, des opérations plus complexes peuvent bénéficier de bibliothèques ou outils tiers. Cependant, pour la manipulation standard de CSV, telle que la lecture, l'écriture, le filtrage ou le tri, les cmdlets intégrés de PowerShell comme `Import-Csv` et `Export-Csv` offrent généralement une fonctionnalité robuste sans nécessiter de bibliothèques supplémentaires.

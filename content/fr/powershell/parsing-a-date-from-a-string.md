---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analyser une Date à partir d'une Chaîne de Caractères avec PowerShell

## Qu'est-ce que c'est & Pourquoi ?

L'analyse d'une date à partir d'une chaîne de caractères en programmation consiste à convertir un texte formaté en date. C'est une opération nécessaire étant donné que les dates sont souvent traitées comme du texte dans les fichiers ou les bases de données.

## Comment faire

Voici comment vous pouvez analyser une date à partir d'une chaîne de caractères avec PowerShell :

```PowerShell
# Définition de la chaîne de date
$dateString = "2022-03-30"

# Parse la date
$parsedDate = [DateTime]::Parse($dateString)

# Affiche la date
$parsedDate
```

Ceci produira la sortie suivante :

```PowerShell
Mercredi 30 mars 2022 00:00:00
```

## Approfondissement

### Contexte historique

Avant PowerShell, les développeurs de scripts étaient souvent limités à l'analyse manuelle des chaînes de date par des méthodes de chaîne traditionnelles en VBScript ou en utilisant la méthode Parse de DateTime du .NET Framework dans les scripts C#.

### Alternatives

PowerShell offre également la méthode `ParseExact` pour analyser les dates avec un format spécifique. Cela peut être utile si votre entrée ne suit pas un format couramment utilisé.

```PowerShell
# Format spécifique
$format = "dd-MM-yyyy"

# Chaîne de date spécifique
$dateString = "30-03-2022"

# Parse la date
$parsedDate = [DateTime]::ParseExact($dateString, $format, $null)

# Affiche la date
$parsedDate
```

### Détails d'implémentation

L'analyse d'une date à partir d'une chaîne utilise la méthode Parse ou ParseExact de la classe DateTime du .NET Framework. Dans l'exemple ci-dessus, `$null` est utilisé pour la culture car nous n'utilisons pas de culture spécifique.

## Voir aussi

Pour en savoir plus sur les opérations de date et d'heure avec PowerShell, consultez ces ressources :

- Guide officiel MS : [tout ce que vous devez savoir sur les dates](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-6.0)
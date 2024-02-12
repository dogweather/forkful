---
title:                "Utilisation des tableaux associatifs"
aliases:
- fr/powershell/using-associative-arrays.md
date:                  2024-01-30T19:12:33.726998-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Les tableaux associatifs, également connus sous le nom de tables de hachage ou de dictionnaires dans PowerShell, vous permettent de stocker des données en paires clé-valeur, rendant la récupération des données simple et efficace. Les programmeurs les utilisent pour stocker des données liées ensemble de manière facilement accessible par clé.

## Comment faire :

Créer et utiliser des tableaux associatifs dans PowerShell est assez simple. Voici comment procéder :

**Créer un tableau associatif :**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Ingénieur"
```

Ce morceau de code crée un tableau associatif avec trois paires clé-valeur.

**Accéder aux valeurs :**

Pour obtenir une valeur, référencez sa clé :

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**Exemple de sortie :**

```
Alex
```

**Ajouter ou modifier des données :**

Utilisez simplement la clé pour ajouter une nouvelle paire ou modifier une existante :

```PowerShell
$myAssociativeArray["location"] = "New York" # Ajoute une nouvelle paire clé-valeur
$myAssociativeArray["job"] = "Ingénieur Senior" # Modifie une paire existante
```

**Itérer sur un tableau associatif :**

Bouclez à travers les clés et les valeurs comme ceci :

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**Exemple de sortie :**

```
name : Alex
age : 25
job : Ingénieur Senior
location : New York
```

## Plongée en Profondeur

Le concept des tableaux associatifs est courant dans de nombreux langages de programmation, généralement appelé un dictionnaire, une carte ou une table de hachage selon le langage. Dans PowerShell, les tableaux associatifs sont implémentés comme des tables de hachage, qui sont assez efficaces pour la recherche de clés, le stockage de données et le maintien d'une collection de clés uniques.

Historiquement, les tableaux associatifs fournissent un moyen de gérer des collections d'objets où chaque élément peut être rapidement récupéré sans parcourir toute la collection, en utilisant sa clé. L'efficacité de la récupération et de la modification des données dans les tableaux associatifs en fait un choix privilégié pour diverses tâches. Cependant, ils ont des limitations, telles que le maintien de l'ordre, pour lesquels les dictionnaires ordonnés ou les objets personnalisés pourraient être une meilleure alternative.

Malgré leurs limitations, les tableaux associatifs/tables de hachage dans PowerShell sont incroyablement flexibles et un outil puissant pour le scripting. Ils permettent un stockage de données dynamique et sont particulièrement utiles dans les configurations, la manipulation de données, et partout où un format de données structurées est nécessaire sans les frais généraux d'une définition de classe formelle. Rappelez-vous juste, tandis que les tableaux associatifs sont parfaits pour la récupération basée sur la clé, si votre tâche implique des structures de données complexes ou nécessite de maintenir un ordre spécifique, vous pourriez vouloir explorer d'autres types de données ou objets personnalisés dans PowerShell.

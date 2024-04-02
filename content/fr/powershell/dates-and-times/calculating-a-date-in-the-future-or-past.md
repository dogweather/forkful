---
date: 2024-01-20 17:31:45.818745-07:00
description: "Calculer une date dans le futur ou le pass\xE9 permet de d\xE9terminer\
  \ un moment pr\xE9cis relatif \xE0 aujourd'hui. Les programmeurs l'utilisent pour\
  \ g\xE9rer les\u2026"
lastmod: '2024-03-13T22:44:58.069831-06:00'
model: gpt-4-1106-preview
summary: "Calculer une date dans le futur ou le pass\xE9 permet de d\xE9terminer un\
  \ moment pr\xE9cis relatif \xE0 aujourd'hui. Les programmeurs l'utilisent pour g\xE9\
  rer les\u2026"
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Quoi & Pourquoi ?
Calculer une date dans le futur ou le passé permet de déterminer un moment précis relatif à aujourd'hui. Les programmeurs l'utilisent pour gérer les échéances, rappels ou planifier des événements.

## Comment faire :
```PowerShell
# Ajouter 10 jours à la date d'aujourd'hui
$dateDans10Jours = (Get-Date).AddDays(10)
Write-Host "Dans 10 jours, nous serons le $dateDans10Jours"

# Retirer 20 jours à partir d'aujourd'hui
$dateIlY20Jours = (Get-Date).AddDays(-20)
Write-Host "Il y a 20 jours, c'était le $dateIlY20Jours"
```
```
Dans 10 jours, nous serons le 12 avril 2023
Il y a 20 jours, c'était le 13 mars 2023
```

## Exploration en profondeur
Historiquement, la manipulation des dates a été source de multiples erreurs à cause de formats incohérents et de la gestion des fuseaux horaires. PowerShell simplifie cette tâche avec `[DateTime]` et des méthodes comme `AddDays()`. En plus de `AddDays()`, il existe `AddHours()`, `AddMinutes()`, pour plus de précision. En alternative, on peut aussi utiliser `.NET` avec `[System.DateTime]` ou autres langages comme Python, qui offre `datetime.timedelta`, mais PowerShell reste un choix robuste pour les scripts d'administration système.

## Voir aussi
- Documentation Microsoft pour `[DateTime]` : [https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime)
- Tutoriel sur la manipulation de dates en PowerShell : [https://ss64.com/ps/syntax-dateformats.html](https://ss64.com/ps/syntax-dateformats.html)
- Comparaison des fonctionnalités de date/heure dans différents langages de programmation : [https://en.wikipedia.org/wiki/System_time](https://en.wikipedia.org/wiki/System_time)

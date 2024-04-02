---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:29.232157-07:00
description: "R\xE9cup\xE9rer la date courante dans PowerShell consiste \xE0 obtenir\
  \ la date et l'heure actuelles du syst\xE8me. Cette op\xE9ration est fondamentale\
  \ pour des t\xE2ches\u2026"
lastmod: '2024-03-13T22:44:58.064587-06:00'
model: gpt-4-0125-preview
summary: "R\xE9cup\xE9rer la date courante dans PowerShell consiste \xE0 obtenir la\
  \ date et l'heure actuelles du syst\xE8me. Cette op\xE9ration est fondamentale pour\
  \ des t\xE2ches\u2026"
title: Obtenir la date actuelle
weight: 29
---

## Quoi et Pourquoi ?

Récupérer la date courante dans PowerShell consiste à obtenir la date et l'heure actuelles du système. Cette opération est fondamentale pour des tâches telles que la journalisation, le chronométrage d'opérations ou la prise de décisions basées sur des dates. Les programmeurs utilisent cette capacité pour suivre des événements, planifier des tâches et gérer la logique spécifique aux dates dans des scripts et applications.

## Comment faire :

PowerShell fournit des cmdlets simples pour obtenir la date et l'heure. Le cmdlet `Get-Date` est l'outil principal à cet effet. Il peut retourner la date et l'heure complètes, que vous pouvez formater ou manipuler selon vos besoins.

```powershell
# Obtenir la date et l'heure actuelles
Get-Date
```

**Exemple de sortie :**

```
Tuesday, September 5, 2023 9:46:02 AM
```

Vous pouvez également formater la sortie pour afficher uniquement les informations dont vous avez besoin, comme juste la date ou juste l'heure.

```powershell
# Obtenir uniquement la date actuelle dans un format spécifique
Get-Date -Format "yyyy-MM-dd"
```

**Exemple de sortie :**

```
2023-09-05
```

```powershell
# Obtenir uniquement l'heure actuelle
Get-Date -Format "HH:mm:ss"
```

**Exemple de sortie :**

```
09:46:02
```

### Utiliser la classe .NET

PowerShell permet un accès direct aux classes .NET, offrant une manière alternative de travailler avec les dates et heures.

```powershell
# Utiliser la classe DateTime .NET pour obtenir la date et l'heure actuelles
[System.DateTime]::Now
```

**Exemple de sortie :**

```
Tuesday, September 5, 2023 9:46:02 AM
```

Pour l'heure UTC :

```powershell
# Utiliser la classe DateTime .NET pour obtenir la date et l'heure UTC actuelles
[System.DateTime]::UtcNow
```

**Exemple de sortie :**

```
Tuesday, September 5, 2023 1:46:02 PM
```

Ces commandes et classes fournissent des options puissantes et flexibles pour travailler avec les dates et les heures dans PowerShell, essentielles pour de nombreuses tâches de script et d'automatisation.

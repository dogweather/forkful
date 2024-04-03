---
date: 2024-01-20 17:44:36.309734-07:00
description: "How to: PowerShell rend le t\xE9l\xE9chargement simple. La commande\
  \ `Invoke-WebRequest` est votre outil. Voici un exemple ."
lastmod: '2024-03-13T22:44:58.046241-06:00'
model: gpt-4-1106-preview
summary: "PowerShell rend le t\xE9l\xE9chargement simple."
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to:
PowerShell rend le téléchargement simple. La commande `Invoke-WebRequest` est votre outil. Voici un exemple :

```PowerShell
# Télécharge le contenu de la page d'accueil de Example.com et l'affiche dans la console
$response = Invoke-WebRequest -Uri 'http://example.com'
Write-Output $response.Content
```

Vous verrez le HTML de la page s'afficher. Facile, non ?

## Deep Dive
Avant PowerShell, on utilisait d'autres scripts ou programmes pour télécharger des pages web. Par exemple, `wget` et `curl` sont toujours populaires pour ces tâches, surtout hors de Windows.

PowerShell s'est ajouté au jeu avec `Invoke-WebRequest`. Ça permet de faire plus que télécharger : on peut aussi interagir avec le web. Par exemple, envoyer des formulaires ou gérer des sessions.

Les détails d'implémentation ? `Invoke-WebRequest` fonctionne bien avec le reste de PowerShell. On peut chaîner des commandes, filtrer, trier – bref, utiliser toute la puissance de PowerShell avec vos données web.

## See Also
- La documentation officielle de `Invoke-WebRequest` : [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)

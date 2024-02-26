---
date: 2024-01-20 17:44:36.309734-07:00
description: "T\xE9l\xE9charger une page web, c'est r\xE9cup\xE9rer son contenu via\
  \ le net. Les programmeurs font \xE7a pour analyser les donn\xE9es, automatiser\
  \ des t\xE2ches, ou\u2026"
lastmod: '2024-02-25T18:49:54.729049-07:00'
model: gpt-4-1106-preview
summary: "T\xE9l\xE9charger une page web, c'est r\xE9cup\xE9rer son contenu via le\
  \ net. Les programmeurs font \xE7a pour analyser les donn\xE9es, automatiser des\
  \ t\xE2ches, ou\u2026"
title: "T\xE9l\xE9chargement d'une page web"
---

{{< edit_this_page >}}

## What & Why?
Télécharger une page web, c'est récupérer son contenu via le net. Les programmeurs font ça pour analyser les données, automatiser des tâches, ou simplement pour sauvegarder des infos.

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

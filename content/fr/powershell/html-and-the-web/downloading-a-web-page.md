---
title:                "Téléchargement d'une page web"
aliases:
- /fr/powershell/downloading-a-web-page/
date:                  2024-01-20T17:44:36.309734-07:00
model:                 gpt-4-1106-preview
simple_title:         "Téléchargement d'une page web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/downloading-a-web-page.md"
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

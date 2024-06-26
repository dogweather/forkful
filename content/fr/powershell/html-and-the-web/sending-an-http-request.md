---
date: 2024-01-20 18:00:36.038726-07:00
description: "How to: Faire une requ\xEAte HTTP simple en PowerShell ? C'est assez\
  \ direct. Utilisez `Invoke-RestMethod` ou `Invoke-WebRequest`. Voil\xE0 deux exemples."
lastmod: '2024-04-05T22:40:44.795287-06:00'
model: gpt-4-1106-preview
summary: "Faire une requ\xEAte HTTP simple en PowerShell ?"
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## How to:
Faire une requête HTTP simple en PowerShell ? C'est assez direct. Utilisez `Invoke-RestMethod` ou `Invoke-WebRequest`. Voilà deux exemples:

```PowerShell
# Obtenez des données JSON d'une API
$response = Invoke-RestMethod -Uri 'https://api.exemple.com/data' -Method Get
$response | ConvertTo-Json

# Postez des données vers une API
$body = @{nom='Dupont'; prenom='Jean'} | ConvertTo-Json
Invoke-RestMethod -Uri 'https://api.exemple.com/user' -Method Post -Body $body -ContentType 'application/json'
```
Et pour voir les résultats :
```PowerShell
# Affichez les détails de la réponse après une requête Get
$response

# Résultat après le Post, souvent la ressource créée ou un message de succès
# (dépend de l'API)
```

## Deep Dive
Les requêtes HTTP n'ont pas toujours été aussi simples en PowerShell. Avant la commande `Invoke-RestMethod`, introduite dans la version 3.0, on utilisait `System.Net.WebClient` ou `System.Net.HttpWebRequest`. Ces classes .NET sont toujours disponibles mais `Invoke-RestMethod` est plus haut niveau et facile à utiliser.

Pourquoi choisir l'une sur l'autre ? `Invoke-RestMethod` est idéale pour travailler avec des API RESTful. Elle parse automatiquement le JSON ou XML. En revanche, `Invoke-WebRequest` donne plus de contrôle sur la requête HTTP elle-même – utile si vous avez besoin des headers, du status, etc.

Dernier point, pour une application asynchrone ou très performante, envisagez de passer par les fonctionnalités asynchrones dans .NET avec `HttpClient`.

## See Also
- [Documentation officielle de PowerShell](https://docs.microsoft.com/fr-fr/powershell/)
- [Invoke-RestMethod](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7)
- [Invoke-WebRequest](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [Sur le blog de PowerShell](https://devblogs.microsoft.com/powershell/)

Bon codage avec PowerShell et les requêtes HTTP !

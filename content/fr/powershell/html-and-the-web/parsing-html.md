---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:39.867489-07:00
description: "Analyser du HTML en PowerShell consiste \xE0 diss\xE9quer un contenu\
  \ HTML pour extraire des donn\xE9es sp\xE9cifiques ou pour automatiser des t\xE2\
  ches li\xE9es au web. Les\u2026"
lastmod: '2024-03-13T22:44:58.044845-06:00'
model: gpt-4-0125-preview
summary: "Analyser du HTML en PowerShell consiste \xE0 diss\xE9quer un contenu HTML\
  \ pour extraire des donn\xE9es sp\xE9cifiques ou pour automatiser des t\xE2ches\
  \ li\xE9es au web."
title: Analyse Syntaxique du HTML
weight: 43
---

## Comment faire :
PowerShell n'a pas nativement un parseur HTML dédié, mais vous pouvez utiliser le cmdlet `Invoke-WebRequest` pour accéder et analyser le contenu HTML. Pour un analyse plus complexe et manipulation, HtmlAgilityPack, une bibliothèque .NET populaire, peut être utilisée.

### Utilisation de `Invoke-WebRequest` :
```powershell
# Exemple simple pour récupérer les titres d'une page web
$response = Invoke-WebRequest -Uri 'http://example.com'
# Utiliser la propriété ParsedHtml pour accéder aux éléments DOM
$title = $response.ParsedHtml.title
Write-Output $title
```

Exemple de sortie :

```
Domaine Exemple
```

### Utilisation d'HtmlAgilityPack :
Tout d'abord, vous devez installer HtmlAgilityPack. Vous pouvez le faire via le Gestionnaire de Packages NuGet :

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

Ensuite, vous pouvez l'utiliser dans PowerShell pour analyser le HTML :

```powershell
# Charger l'assemblage HtmlAgilityPack
Add-Type -Path "chemin\vers\HtmlAgilityPack.dll"

# Créer un objet HtmlDocument
$doc = New-Object HtmlAgilityPack.HtmlDocument

# Charger le HTML à partir d'un fichier ou d'une requête web
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# Utiliser XPath ou d'autres méthodes de requête pour extraire des éléments
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

Exemple de sortie :

```
Bienvenue sur Example.com !
```

Dans ces exemples, `Invoke-WebRequest` est mieux adapté pour des tâches simples, tandis qu'HtmlAgilityPack offre un ensemble de fonctionnalités beaucoup plus riche pour une analyse et manipulation HTML complexe.

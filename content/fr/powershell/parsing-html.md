---
title:                "Analyse Syntaxique du HTML"
date:                  2024-02-03T19:12:39.867489-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyse Syntaxique du HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser du HTML en PowerShell consiste à disséquer un contenu HTML pour extraire des données spécifiques ou pour automatiser des tâches liées au web. Les programmeurs le font pour interagir avec des pages web, récupérer du contenu web ou automatiser la soumission de formulaires et d'autres interactions web sans avoir besoin d'un navigateur web.

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

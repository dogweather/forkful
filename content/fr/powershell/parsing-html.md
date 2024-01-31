---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:33:11.218561-07:00
simple_title:         "Analyse syntaxique de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Le parsing HTML, c'est transformer le code HTML en un format exploitable par les scripts. Les programmeurs l'utilisent pour automatiser l'analyse et l'extraction d'informations des pages web.

## How to:
PowerShell n'a pas de fonctionnalité de parsing HTML intégrée, mais on peut utiliser le module HtmlAgilityPack. Installez-le via NuGet et voyons comment l'utiliser avec un exemple simple :

```PowerShell
# Installer HtmlAgilityPack
Install-Package HtmlAgilityPack -Scope CurrentUser

# Utilisation de HtmlAgilityPack pour parser un document HTML
$html = New-Object HtmlAgilityPack.HtmlDocument
$html.LoadHtml("<html><body><p>Salut le monde !</p></body></html>")

# Extraction du contenu du paragraphe
$pContent = $html.DocumentNode.SelectSingleNode("//p").InnerText
Write-Host $pContent
```

Sortie :
```
Salut le monde!
```

## Deep Dive
Avant HtmlAgilityPack et d'autres bibliothèques similaires, les techniques de parsing HTML étaient rudimentaires et impliquaient souvent l'utilisation d'expressions régulières, ce qui n'est pas idéal étant donné la complexité et la variabilité du HTML. HtmlAgilityPack résout ce problème en fournissant un modèle d'objet document (DOM) que vous pouvez interroger avec XPath. Ses alternatives incluent AngleSharp ou des approches côté client comme JavaScript avec la méthode `document.querySelectorAll`. Les détails d'implémentation avec HtmlAgilityPack impliquent que le DOM HTML est chargé dans un objet facile à explorer et à manipuler. Cela vous permet de réaliser des opérations CRUD (Créer, Lire, Mettre à jour, Supprimer) sur le contenu HTML.

## See Also
- HtmlAgilityPack Documentation: https://html-agility-pack.net/?z=codeplex
- XPath Syntax: https://www.w3schools.com/xml/xpath_syntax.asp
- AngleSharp GitHub Repository: https://github.com/AngleSharp/AngleSharp

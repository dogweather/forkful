---
title:                "Analyse de code html"
html_title:           "PowerShell: Analyse de code html"
simple_title:         "Analyse de code html"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?
Le HTML, abréviation pour "HyperText Markup Language", est un langage utilisé pour créer et structurer des pages web. Le "parsing" HTML, c'est lorsque l'on analyse et manipule le code HTML d'une page web pour extraire des informations spécifiques. Les programmeurs le font souvent pour automatiser des tâches ou extraire des données d'un site web.

## Comment faire:
Voici un exemple simple de "parsing" HTML avec PowerShell:

```PowerShell
# On utilise la cmdlet Invoke-WebRequest pour récupérer le code HTML d'une page web
$page = Invoke-WebRequest -Uri "https://www.example.com"

# On utilise ensuite la propriété "ParsedHtml" pour accéder à l'objet HTML de la page
# et la méthode "GetElementsByTagName" pour sélectionner tous les éléments d'une certaine balise
$paragraphes = $page.ParsedHtml.GetElementsByTagName("p")

# On peut ensuite parcourir les éléments et afficher leur contenu
foreach ($p in $paragraphes) {
    Write-Host $p.InnerText
}
```

Output:
```
Ceci est le contenu d'un paragraphe.
Ceci est un autre paragraphe.
```

## Profondeur de plongée:
L'analyse HTML existe depuis la création du web dans les années 90. Avant PowerShell, les programmeurs devaient utiliser des langages tels que Perl ou Python pour effectuer cette tâche. Aujourd'hui, il existe également des modules dédiés au "parsing" HTML tels que "HtmlAgilityPack" pour .NET. Dans certains cas, il peut être plus approprié d'utiliser ces alternatives en fonction des besoins spécifiques du projet.

Sur le plan technique, lorsqu'un programme "parse" le HTML d'une page web, il crée un "Document Object Model" (DOM) de cette page. Cela permet de naviguer et d'extraire des données précises en utilisant des propriétés et méthodes du DOM.

## Voir aussi:
- [Invoke-WebRequest](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- [HtmlAgilityPack](https://html-agility-pack.net/)
- [Document Object Model](https://developer.mozilla.org/fr/docs/Web/API/Document_Object_Model/Introduction)
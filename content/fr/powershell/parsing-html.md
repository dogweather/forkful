---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La syntaxe HTML est convertie ('parsée') en structures de données par un analyseur HTML. Les programmeurs le font pour extraire des informations spécifiques, manipuler des pages Web ou même construire des crawlers Web.

## Mode d'emploi:
Pour parser du HTML dans PowerShell, on utilise un objet HTMLDocument avec la méthode 'InvokeScript'. Regardez cet exemple :

```PowerShell
# Ajouter un type pour interagir avec Internet Explorer 
Add-Type -AssemblyName Microsoft.mshtml
# Charger le HTML dans un objet HTMLDocument
$html = New-Object -ComObject "HTMLFile"
$html.IHTMLDocument2_write($(Invoke-WebRequest -uri “https://votre-site-web.com”))

# Extraire un élément par son ID
$element = $html.getElementById('votre_id')

# Afficher le texte de l'élément
$element.innerText
```
Ce code va télécharger le HTML, le parser, trouver un élément par son identifiant et imprimera son texte en sortie.

## Plongée en profondeur
Historiquement, HTML était moins structuré et plus souple que le XML, causant des problèmes pour parser certains documents. Avec HTML5, le HTML a adopté une syntaxe plus stricte facilitant le parsing.

Il y a plusieurs alternatives à PowerShell pour parser HTML, notamment BeautifulSoup pour Python ou Cheerio pour Node.js. Ces outils offrent plus de fonctionnalités mais nécessitent l'installation d'un environnement de programmation distinct.

La méthode InvokeScript exécute un bloc de script, le rendant idéal pour interagir avec une page web en utilisant son DOM.

## À voir également
- [Window.IHTMLDocument2_write method - Microsoft Docs](https://docs.microsoft.com/en-us/previous-versions/windows/desktop/ms633519(v=vs.85))
- [BeautifulSoup Python Library](https://www.crummy.com/software/BeautifulSoup/)
- [Cheerio Node.js Library](https://cheerio.js.org)
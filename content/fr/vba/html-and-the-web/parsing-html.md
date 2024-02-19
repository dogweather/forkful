---
aliases:
- /fr/vba/parsing-html/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:12.879172-07:00
description: "L'analyse syntaxique du HTML dans Visual Basic pour Applications (VBA)\
  \ consiste \xE0 extraire des informations sp\xE9cifiques d'un document HTML. Les\u2026"
lastmod: 2024-02-18 23:09:08.575816
model: gpt-4-0125-preview
summary: "L'analyse syntaxique du HTML dans Visual Basic pour Applications (VBA) consiste\
  \ \xE0 extraire des informations sp\xE9cifiques d'un document HTML. Les\u2026"
title: Analyse Syntaxique de HTML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'analyse syntaxique du HTML dans Visual Basic pour Applications (VBA) consiste à extraire des informations spécifiques d'un document HTML. Les programmeurs le font pour automatiser le processus de lecture et de manipulation des données provenant des pages web, telles que le scraping de contenu de sites ou l'automatisation de la soumission de formulaires et de la récupération de données, dans des applications comme Microsoft Excel ou Access qui prennent en charge VBA.

## Comment faire :

Dans VBA, vous pouvez analyser le HTML en utilisant la `Bibliothèque d'objets HTML Microsoft`. Ajoutez une référence à cette bibliothèque dans votre éditeur VBA en allant dans Outils > Références et en cochant `Bibliothèque d'objets HTML Microsoft`. Cela vous donne accès à des classes pour naviguer et manipuler des documents HTML.

Voici un exemple simple qui montre comment charger un document HTML à partir d'un fichier et extraire tous les liens (balises d'ancre) :

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' Charger le contenu HTML à partir d'un fichier
    htmlFile = "C:\chemin\vers\votre\fichier.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' Initialiser le Document HTML
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' Obtenir toutes les balises d'ancre
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Répéter pour tous les éléments d'ancre et imprimer l'attribut href
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Ce script lit le contenu d'un fichier HTML, le charge dans un objet `HTMLDocument`, récupère tous les éléments d'ancre (`<a>` tags), puis les parcourt, en imprimant l'attribut `href` de chacun dans la fenêtre Immédiate.

## Exploration approfondie :

Historiquement, l'analyse syntaxique du HTML dans VBA a été un peu laborieuse en raison du manque de support direct pour les technologies modernes de scraping web et de manipulation de documents. La Bibliothèque d'objets HTML Microsoft, bien que puissante, est quelque peu datée et peut ne pas gérer les normes web modernes aussi bien que les technologies plus récentes.

Pour des tâches complexes d'analyse syntaxique HTML et de scraping web, des outils et langages alternatifs tels que Python avec des bibliothèques telles que Beautiful Soup ou Scrapy sont souvent recommandés. Ces outils modernes offrent plus de flexibilité, une meilleure performance et sont plus en phase avec les normes web actuelles. Cependant, lorsqu'on travaille dans l'écosystème Microsoft Office, l'utilisation de VBA avec la Bibliothèque d'objets HTML Microsoft reste une compétence précieuse. Elle ouvre la voie à la manipulation directe du contenu HTML d'une manière qui s'intègre parfaitement avec des applications comme Excel et Access, offrant une méthode directe pour accomplir des tâches qui impliquent la manipulation de documents HTML basiques sans avoir à sortir de l'environnement VBA familier.

---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?
Le parsing HTML est le processus de traduction du texte brut HTML en éléments structurés et compréhensibles pour le programme. Les développeurs les utilisent pour extraire des données d'HTML, automatiser des actions sur le web ou pour créer du web scraping.

## Comment faire:
Voici comment vous pouvez utiliser la bibliothèque `HtmlAgilityPack` pour analyser du HTML.

Installation du paquet Nuget.
```C#
Install-Package HtmlAgilityPack
```

Voici un exemple basique de parsing HTML en C#.

```C#
using HtmlAgilityPack;
HtmlWeb web = new HtmlWeb();
HtmlDocument doc = web.Load("http://exemple.com");

var nodes = doc.DocumentNode.SelectNodes("//p");

foreach (var node in nodes)
{
    Console.WriteLine(node.InnerHtml);
}
```
Un exemple de sortie pourrait être:

```C#
<div class='mon_div'>Bonjour Monde!</div>
<div class='autre_div'>Voici un autre div.</div>
```

## Un peu plus de perspective:
Le parsing HTML est devenu vital avec l'augmentation du web scraping et de l'automatisation web. Il a été utilisé pour la première fois dans les années 90 lors de la montée des navigateurs web. 

En C#, nous avons des alternatives comme `CsQuery` qui fonctionne comme jQuery pour C# et `AngleSharp` qui est une implémentation DOM compétente. Choisir l'un d'eux dépend des besoins spécifiques de votre projet.

Lors de votre choix, pensez à vérifier facteurs comme: le support des standards web, la vitesse d'exécution, la documentation, la compatibilité avec différentes versions de .NET, etc.

## Voir aussi :
Visiter ces liens pour plus d'informations :

- Documentation HtmlAgilityPack : https://html-agility-pack.net/documentation
- Documentation CsQuery : https://github.com/jamietre/CsQuery
- Documentation AngleSharp : https://anglesharp.github.io/
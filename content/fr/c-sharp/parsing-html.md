---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:30:37.767643-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi?)
Parser du HTML, c'est lire et comprendre le code d'une page web pour en extraire des données. Les programmeurs le font pour automatiser la collecte d'informations, analyser le contenu ou intégrer des fonctionnalités web dans leurs applis.

## How to (Comment faire):
Utilisons HtmlAgilityPack, une bibliothèque C# populaire pour le parsing HTML.

```C#
using HtmlAgilityPack;

var web = new HtmlWeb();
var document = web.Load("https://exemple.com");
var nodes = document.DocumentNode.SelectNodes("//h2");

foreach (var node in nodes)
{
    Console.WriteLine(node.InnerText);
}
```

Sortie attendue:
```
Le titre du premier H2
Le titre du second H2
...
```

## Deep Dive (Plongée Profonde):
Historiquement, parser du HTML était compliqué en raison de la non-standardisation et de la latitude laissée aux développeurs web. HtmlAgilityPack a été une révolution, offrant une interface similaire à XPath/XML pour manipuler du HTML. 

Alternatives : AngleSharp est une autre bibliothèque moderne qui respecte les derniers standards du Web.

Détail d'implémentation : HtmlAgilityPack gère les HTML mal formés, corrigeant les erreurs communes pour permettre un parcours du DOM efficace.

## See Also (Voir Aussi):
- HtmlAgilityPack sur NuGet: https://www.nuget.org/packages/HtmlAgilityPack
- Documentation HtmlAgilityPack: https://html-agility-pack.net/
- AngleSharp sur GitHub: https://github.com/AngleSharp/AngleSharp

---
title:                "Analyser l'html"
html_title:           "C#: Analyser l'html"
simple_title:         "Analyser l'html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?
Le parsing HTML est un processus qui consiste à extraire des données structurées à partir de code HTML. Les programmeurs utilisent le parsing HTML pour automatiser la collecte et l'analyse de données sur les pages web. Cela peut être particulièrement utile pour effectuer des tâches telles que le web scraping, la création de bots ou l'analyse de données.

## Comment faire :
Voici un exemple de code en C# pour effectuer un parsing HTML à l'aide de la librairie HtmlAgilityPack. Tout d'abord, il faut référencer la librairie dans le projet. Ensuite, il suffit de créer un objet HtmlDocument et de lui passer en paramètre le contenu de la page web à parser. Enfin, on peut utiliser des méthodes telles que SelectNodes ou SelectSingleNode pour cibler les éléments spécifiques que l'on souhaite extraire.

```C#
using HtmlAgilityPack;

HtmlDocument doc = new HtmlDocument();
doc.LoadHtml("contenu de la page à parser");

// exemple d'extraction des balises <a> contenant un lien
var links = doc.DocumentNode.SelectNodes("//a[@href]");

foreach (HtmlNode link in links)
{
    var url = link.Attributes["href"].Value;
    Console.WriteLine(url);
}
```

L'exemple ci-dessus montre comment extraire tous les liens présents sur une page web. Il est également possible d'utiliser des conditions pour cibler des éléments plus précis ou de manipuler les données extraites selon nos besoins.

## Plongée en profondeur :
Le parsing HTML est largement utilisé dans le domaine du web scraping, qui a vu un essor considérable ces dernières années avec l'augmentation des données disponibles en ligne. Il existe d'autres outils pour réaliser du parsing, tels que Python Beautiful Soup ou JavaScript Cheerio, mais la librairie HtmlAgilityPack reste une référence pour les développeurs C#.

L'implémentation du parsing est souvent complexe en raison de la variété de syntaxes HTML. Il est donc important de bien comprendre la structure des pages que l'on souhaite parser avant de se lancer dans le code. De plus, il est important de prendre en compte les éventuelles mises à jour du code HTML au fil du temps, qui peuvent casser notre parsing si celui-ci est trop rigide.

## Voir aussi :
- [Documentation officielle de HtmlAgilityPack](https://html-agility-pack.net/)
- [Article sur le web scraping avec C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/tutorials/scraping-html)
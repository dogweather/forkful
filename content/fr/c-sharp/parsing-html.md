---
title:                "C#: Analyse de l'html"
simple_title:         "Analyse de l'html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

La programmation est un art qui consiste à donner des instructions à un ordinateur pour effectuer des tâches précises. Dans certaines situations, il est nécessaire d'extraire des informations spécifiques d'une page web, par exemple des données de vente ou des informations d'un produit. C'est là que le parsing HTML entre en jeu.

## Comment faire

Le parsing HTML consiste à analyser une page web et à extraire des informations en fonction de balises spécifiques. Cela peut sembler compliqué, mais avec le langage de programmation C#, c'est assez simple. Voici un exemple de code pour récupérer le titre d'une page HTML:

```C#
// Importer les librairies nécessaires
using System;
using System.Net;
using HtmlAgilityPack; // Cette librairie nous permet de naviguer et de parser un document HTML

// Définir l'URL de la page à parser
string url = "https://www.example.com";

// Créer un objet WebRequest pour télécharger la page HTML
WebRequest req = HttpWebRequest.Create(url);

// Récupérer la réponse de la requête
WebResponse res = req.GetResponse();

// Lire la réponse en tant que flux de données
var stream = res.GetResponseStream();

// Créer un objet pour parser le document HTML téléchargé
var doc = new HtmlDocument();
doc.Load(stream);

// Utiliser la méthode SelectSingleNode pour trouver la balise "title" et extraire son contenu
var title = doc.DocumentNode.SelectSingleNode("//title").InnerText;

Console.WriteLine($"Le titre de la page est : {title}"); 
```

Ce code utilise la librairie HtmlAgilityPack pour parser le document HTML téléchargé à partir de l'URL spécifiée. En utilisant la méthode SelectSingleNode, nous pouvons rechercher une balise spécifique et extraire son contenu.

La sortie de ce code sera quelque chose comme ceci : "Le titre de la page est : Example".

Bien sûr, ce n'est qu'un exemple simple, mais vous pouvez appliquer cette même méthode pour extraire toutes sortes d'informations à partir d'une page HTML.

## Plongée profonde

Pour ceux qui souhaitent approfondir leurs connaissances en parsing HTML avec C#, il existe de nombreuses ressources disponibles en ligne. La librairie HtmlAgilityPack est très populaire et dispose d'une documentation complète avec des exemples de code. Il y a également d'autres librairies telles que AngleSharp et CsQuery qui offrent des fonctionnalités similaires.

Il est également important de comprendre les concepts de base du langage HTML pour être en mesure de cibler et d'extraire les bonnes balises dans un document. Un bon point de départ est de se familiariser avec la structure des balises HTML et leurs attributs.

## Voir aussi

- [Documentation HtmlAgilityPack](https://html-agility-pack.net/documentation)
- [Tutoriel sur le parsing HTML en C#](https://www.codeguru.com/columns/csharp/using-htmlagilitypack-to-parse-html-in-net-applications.htm)
- [Introduction au parsing HTML en C#](https://www.dotnetperls.com/htmlagilitypack)
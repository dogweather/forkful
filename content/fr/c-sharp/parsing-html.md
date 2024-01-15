---
title:                "L'analyse de HTML"
html_title:           "C#: L'analyse de HTML"
simple_title:         "L'analyse de HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi quelqu'un voudrait-il s'engager dans l'analyse d'HTML ? Eh bien, l'analyse d'HTML est utile pour extraire des données spécifiques d'une page web, telles que des titres, des liens ou des images. Cela peut être très utile pour les développeurs qui cherchent à créer des outils de scraping de données ou pour les utilisateurs qui souhaitent extraire des informations d'un site web pour une utilisation ultérieure.

## Comment faire

L'analyse d'HTML peut sembler intimidante pour certains, mais elle peut être réalisée facilement en utilisant le langage de programmation C# et quelques bibliothèques dédiées.

Tout d'abord, nous devons inclure la bibliothèque HtmlAgilityPack dans notre projet. Cette bibliothèque permet une manipulation facile de documents HTML en utilisant XPath pour naviguer à travers les différents éléments.

```C#
using HtmlAgilityPack;
```

Ensuite, nous devons créer une instance de la classe HtmlDocument en utilisant l'URL de la page que nous voulons analyser comme paramètre.

```C#
HtmlDocument doc = new HtmlWeb().Load("https://monsite.com/pagehtml");
```

Maintenant que nous avons chargé le document, nous pouvons utiliser XPath pour naviguer à travers les différents éléments et extraire les données souhaitées. Par exemple, si nous voulons récupérer tous les titres de la page, nous pouvons utiliser l'expression XPath "//h2" pour sélectionner tous les éléments "h2" et les stocker dans une liste.

```C#
var listeTitres = doc.DocumentNode.SelectNodes("//h2");
```

Enfin, nous pouvons parcourir cette liste et afficher les titres dans la console ou les utiliser comme bon nous semble.

```C#
foreach(var titre in listeTitres)
{
    Console.WriteLine(titre.InnerHtml);
}
```

Voici un exemple de sortie pour une page contenant deux titres "Mon premier titre" et "Mon deuxième titre" :

```
Mon premier titre
Mon deuxième titre
```

## Plongée en profondeur

L'analyse d'HTML peut devenir plus complexe selon les besoins du projet. Heureusement, avec C# et HtmlAgilityPack, il existe de nombreuses options pour traiter et manipuler les données extraites.

Par exemple, nous pouvons utiliser la méthode "RemoveAll" pour supprimer certains éléments de la page avant de la parcourir. Ou encore, nous pouvons utiliser la méthode "GetAttributeValue" pour récupérer un attribut spécifique d'un élément.

Il existe également d'autres bibliothèques en plus de HtmlAgilityPack qui peuvent être utiles pour des besoins spécifiques, telles que AngleSharp qui permet d'émuler un navigateur pour analyser des pages web plus complexes.

Enfin, il est toujours important de connaître les bonnes pratiques en matière d'analyse d'HTML pour éviter les erreurs et optimiser les performances, comme éviter d'utiliser XPath lorsque cela n'est pas nécessaire et vérifier la validité du code HTML ciblé.

## Voir aussi

- Tutoriels pour débutants sur C# : https://csharp.developpez.com/cours/
- Site officiel de HtmlAgilityPack : http://html-agility-pack.net/
- Guide en ligne de XPath : https://www.w3schools.com/xml/xpath_intro.asp
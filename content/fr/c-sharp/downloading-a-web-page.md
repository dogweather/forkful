---
title:                "Le téléchargement d'une page web"
html_title:           "C#: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Télécharger une page web, c'est le fait de récupérer le code HTML d'un site internet et de le stocker sur notre ordinateur ou un autre appareil. Les programmeurs le font pour pouvoir analyser et manipuler le code, ainsi que pour créer des programmes qui peuvent accéder à des données en ligne.

## Comment le faire:
Voici un exemple de code en ```C#``` pour télécharger une page web et en afficher le contenu:

```
using System;
using System.Net;

var url = "https://www.example.com";
using (WebClient client = new WebClient())
{
    string html = client.DownloadString(url);
    Console.WriteLine(html);
}
```

Cela va afficher le code HTML de la page ```https://www.example.com``` dans la console.

## Plongez plus en profondeur:
Historiquement, télécharger une page web était un processus très complexe et laborieux, nécessitant une expertise technique avancée. De nos jours, avec l'avènement de langages de programmation comme ```C#``` et des frameworks tels que .NET, cela peut être fait de manière beaucoup plus efficace et facilement.

Il existe également d'autres alternatives pour télécharger une page web, telles que l'utilisation de bibliothèques de traitement de données telles que Beautiful Soup en Python, ou l'utilisation d'une API spécifique pour accéder aux données souhaitées.

Pour implémenter le téléchargement d'une page web en ```C#```, vous pouvez utiliser la classe ```WebClient``` comme dans l'exemple ci-dessus, ou utiliser une autre bibliothèque telle que HtmlAgilityPack qui offre des fonctionnalités plus avancées telles que la manipulation de données HTML.

## Voir aussi:
- [Documentation officielle de Microsoft sur le téléchargement de pages web en ```C#```](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [Guide complet sur l'utilisation de HtmlAgilityPack en ```C#```](https://www.c-sharpcorner.com/article/scraping-html-documents-using-htmlagilitypack/)
- [Utilisation de Beautiful Soup en Python pour télécharger et extraire des données web](https://www.freecodecamp.org/news/how-to-scrape-websites-with-python-and-beautifulsoup-5946935d93fe/)
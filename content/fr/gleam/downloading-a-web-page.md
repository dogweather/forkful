---
title:                "Téléchargement d'une page web"
html_title:           "Gleam: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Télécharger une page web, c'est récupérer le contenu d'une page web à partir d'une URL donnée. Les programmeurs font cela pour obtenir des données à utiliser dans leurs applications ou pour effectuer des analyses.

## Comment :
Voici un exemple de code en Gleam pour télécharger une page web et afficher son contenu :

```Gleam
url := "https://example.com"
response := Http.get(url)
print(response.body)
```

Output : Le contenu de la page web récupéré sera affiché dans la console.

## Approfondissement :
Les programmeurs utilisent souvent le téléchargement de pages web pour collecter des données à utiliser dans leurs applications ou pour effectuer des tâches telles que le web scraping. Il existe d'autres outils et langages pour effectuer cette tâche, tels que cURL en ligne de commande ou des bibliothèques en JavaScript. Dans Gleam, cela peut être réalisé à l'aide de la bibliothèque standard Http.

## À voir également :
- La documentation de la bibliothèque Http en Gleam : https://gleam.run/packages/http/
- Un tutoriel sur le web scraping en utilisant Gleam : https://medium.com/@perplexeus/using-gleam-to-scrape-the-web-e4897835351c
- Des informations sur d'autres langages et outils pour télécharger des pages web : https://www.lifewire.com/how-to-download-a-website-3481617
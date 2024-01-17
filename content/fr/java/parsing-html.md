---
title:                "Analyse de l'html"
html_title:           "Java: Analyse de l'html"
simple_title:         "Analyse de l'html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faisons-nous?

On peut définir le parsing HTML comme le fait de « lire » un code HTML et d'en extraire des données ou de les manipuler. Les développeurs le font souvent pour exploiter des informations à partir de sites web, automatiser des tâches ou même créer des applications web.

## Comment faire :

Voici un exemple de code Java qui utilise la bibliothèque Jsoup pour parser une page web et en extraire tous les liens :

```Java
Document doc = Jsoup.connect("https://www.example.com").get();
Elements links = doc.select("a");

for (Element link : links) {
    String linkHref = link.attr("href");
    // faire quelque chose avec le lien extrait
}
```

Lorsque vous exécutez ce code, vous obtiendrez une liste de tous les liens présents sur la page web.

## Plongée en profondeur :

Le parsing HTML est né de la nécessité de traiter les données structurées présentes sur les pages web. Avant sa création, les développeurs devaient utiliser des méthodes plus laborieuses pour extraire des données, telles que la recherche manuelle et la copie-coller. Aujourd'hui, il existe des alternatives telles que les API et les bibliothèques de scraping, mais le parsing HTML reste une méthode simple et efficace pour traiter les données web.

## Voir aussi :

- [Documentation de Jsoup](https://jsoup.org/)
- [Différences entre parsing HTML et scraping](https://medium.com/octopus-investments-blog/scraping-vs-parsing-html-data-90de463c3a80)
- [Exemples d'utilisation du parsing HTML en Java](https://www.geeksforgeeks.org/extract-html-links-java/)
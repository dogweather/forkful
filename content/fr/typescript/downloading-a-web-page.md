---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Téléchargement d'une page Web en TypeScript

## Quoi & Pourquoi ?
Télécharger une page web, c'est récupérer le code HTML d'une page à partir d'un serveur. Les développeurs font cela pour analyser le contenu de la page, scraper des données ou tester des fonctionnalités.

## Comment faire :
Pour télécharger une page web en TypeScript, nous pouvons utiliser la bibliothèque `axios`.

```TypeScript
import axios from 'axios';

async function downloadWebPage(url: string): Promise<string> {
  const response = await axios.get(url);
  return response.data;
}

const pageData = downloadWebPage('https://www.google.com');
console.log(pageData);
```

## Plongée en profondeur :
Historiquement, le téléchargement de pages Web a été utilisé pour le "web scraping", qui consiste à extraire des données à partir d'un site web. Aujourd'hui, les développeurs ont également recours à cela pour tester des portions de code ou des services Web.

Il existe plusieurs alternatives à `axios`, comme `node-fetch`, `got` ou `request`. Chaque bibliothèque a ses propres avantages et inconvénients et le choix dépendra de vos besoins spécifiques.

Concernant les détails d'implémentation, `axios` résout une `Promise`. Une `Promise` est une valeur qui peut être disponible maintenant, dans le futur, ou jamais. Dans notre cas, la promesse est résolue par les données de la page Web.

## Voir aussi :
Pour plus d'information concernant le téléchargement de pages Web en TypeScript, vous pouvez visiter les liens suivants :

- Axios sur GitHub : [https://github.com/axios/axios](https://github.com/axios/axios)
- Article de blog sur le web scraping en Node.js : [https://blog.bitsrc.io/https-puppeteering-a-modern-guide-to-scraping-with-headless-browsers-558f7ab25ed2](https://blog.bitsrc.io/https-puppeteering-a-modern-guide-to-scraping-with-headless-browsers-558f7ab25ed2)
- Documentation sur les Promises : [https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Promise](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Promise)
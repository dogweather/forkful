---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:18.042571-07:00
description: "Een nieuw project starten betekent het opzetten van een verse codebasis\
  \ voor je briljante idee\xEBn. Programmeurs doen dit om concepten om te zetten in\u2026"
lastmod: '2024-03-11T00:14:25.040742-06:00'
model: gpt-4-0125-preview
summary: "Een nieuw project starten betekent het opzetten van een verse codebasis\
  \ voor je briljante idee\xEBn. Programmeurs doen dit om concepten om te zetten in\u2026"
title: Een nieuw project starten
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten betekent het opzetten van een verse codebasis voor je briljante ideeën. Programmeurs doen dit om concepten om te zetten in echte, functionerende apps of diensten.

## Hoe:

Voordat je code schrijft, beslis over tools en structuur. Laten we in dit voorbeeld Node.js en npm (Node Package Manager) gebruiken.

1. Installeer Node.js via de [officiële website](https://nodejs.org/).
2. Open een terminal en voer uit:

```javascript
npm init
```

Beantwoord de installatievragen. Bam—`package.json` wordt gecreëerd, hierin wordt je project beschreven. Laten we vervolgens Express toevoegen, een populair web framework:

```javascript
npm install express --save
```

Schrijf nu een eenvoudige webserver in `index.js`:

```javascript
const express = require('express');
const app = express();

app.get('/', (req, res) => {
  res.send('Hallo Wereld!');
});

app.listen(3000, () => {
  console.log('Server draait op poort 3000');
});
```

Start je server:

```javascript
node index.js
```

Voorbeeld van uitvoer:

```
Server draait op poort 3000
```

Ga naar `http://localhost:3000` in je webbrowser. Je zou "Hallo Wereld!" moeten zien.

## Diepere duik

Historisch gezien was het opzetten van een project een pijn, met veel handmatige configuratie. Tegenwoordig nemen tools zoals npm het zware werk over. Voor front-end projecten, overweeg `create-react-app` of `vue-cli`. Voor Node.js is Express een solide keuze, balancerend tussen kracht en eenvoud. Het is lichtgewicht maar heeft robuuste functies voor de meeste webserverbehoeften.

Onthoud, hoe je je project organiseert is cruciaal. Traditionele Node.js-apps hebben een startpunt (zoals `index.js`), een `package.json` bestand om afhankelijkheden te beheren, en een mapstructuur die zaken scheidt (modules, hulpprogramma's, routes, etc.).

Alternatieven voor npm voor pakketbeheer zijn onder andere Yarn, dat snelheids- en consistentieverbeteringen biedt. Voor project scaffolding helpt Yeoman door generators te bieden voor vele soorten projecten en technologieën.

## Zie ook

- Node.js [documentatie](https://nodejs.org/en/docs/)
- Express [officiële site](https://expressjs.com/)
- `create-react-app` [GitHub repo](https://github.com/facebook/create-react-app)
- Vue CLI [documentatie](https://cli.vuejs.org/)
- Yarn [officiële site](https://classic.yarnpkg.com/lang/en/)
- Yeoman [officiële site](http://yeoman.io/)

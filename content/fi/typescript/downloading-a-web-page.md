---
title:                "Verkkosivun lataaminen"
html_title:           "TypeScript: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Verkkosivun lataaminen tarkoittaa yksinkertaisesti sivun sisällön hakemista internetistä ja sen tallentamista omalle tietokoneelle tai järjestelmälle. Ohjelmoijat tekevät tätä usein esimerkiksi siksi, että he haluavat tarkastella sivun sisältöä ja käyttää sitä omassa koodissaan.

## Kuinka tehdä:

Voit käyttää TypeScriptiä verkkosivun lataamiseen seuraavalla tavalla:

```TypeScript
const fetch = require('node-fetch');

fetch('https://example.com')
  .then(res => res.text())
  .then(body => {
    console.log(body);
  });
```

Tämä koodi käyttää `node-fetch` -kirjastoa verkkosivun hakemiseen ja tulostaa sivun sisällön konsoliin.

## Syvemmälle:

Verkkosivun lataaminen on yleisesti ottaen ollut välttämätöntä ohjelmoinnissa jo pitkään, ja tähän tarkoitukseen on kehitetty monia erilaisia kirjastoja ja työkaluja. TypeScript on yksi suosituimmista ohjelmointikielistä tällä hetkellä, joten sen avulla on helppo ladata verkkosivuja ja käsitellä niiden sisältöä.

## Katso myös:

- [Node.js:n virallinen dokumentaatio](https://nodejs.org/en/docs/)
- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/)
- [node-fetch-kirjaston dokumentaatio GitHubissa](https://github.com/node-fetch/node-fetch)
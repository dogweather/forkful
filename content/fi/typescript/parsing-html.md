---
title:                "HTML:n jäsentäminen"
html_title:           "TypeScript: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi analysoida HTML:ää? Yksinkertaisesti sanottuna, HTML-analyysi on tärkeä osa verkkosivujen ja -sovellusten kehittämistä ja ylläpitoa. HTML-analyysin avulla voidaan hakea ja käsitellä verkkosivujen sisältöä, mikä auttaa esimerkiksi tiedon scrapettamisessa, SEO-analyysissä ja testaamisessa.

## Kuinka

HTML-analyysiin on olemassa monia erilaisia tapoja, mutta tässä artikkelissa keskitymme TypeScript-kieleen. TypeScript on suosittu JavaScriptin laajennus, joka tarjoaa vahvemman tyyppitarkastuksen ja muita kehittäjäystävällisiä ominaisuuksia.

```TypeScript
// Tuo tarvittavat moduulit
import * as cheerio from "cheerio";
import * as request from "request";

// Haetaan verkkosivun sisältö
request("https://www.example.com", (error, response, body) => {
  // Avataan sisältö cheerio-kirjaston avulla
  const $ = cheerio.load(body);
  // Haetaan otsikko elementti
  const title = $("h1").text();
  // Tulostetaan otsikko konsolille
  console.log(`Sivun otsikko on: ${title}`);
});
```

Tämä yksinkertainen koodiesimerkki näyttää, kuinka verkkosivun sisältö voidaan hakea ja käsitellä cheerio-kirjaston avulla. Kirjasto tarjoaa samankaltaisen syntaksin kuin jQuery ja helpottaa HTML-elementtien hakemista ja manipulointia verkkosivun sisällöstä.

## Syvällisempi tarkastelu

Vaikka cheerio on yksi suosituimmista kirjastoista HTML-analyysiin TypeScriptillä, on olemassa myös muita vaihtoehtoja, kuten jsdom ja HTML-parseri. Näitä kirjastoja voidaan käyttää samalla tavalla kuin cheeriota ja niitä kannattaa tutkia, jos haluaa vertailla ominaisuuksia ja suorituskykyä.

Lisäksi HTML-analyysin syvällisempi ymmärtäminen auttaa kehittäjiä tehokkaammin hyödyntämään sitä. Esimerkiksi ymmärtämällä CSS-selectorien toimintaa voi helpommin hakea tiettyjä elementtejä verkkosivun sisällöstä. Lisäksi on hyvä tietää, kuinka DOM-puu toimii ja kuinka sitä voidaan käyttää HTML-elementtien manipulointiin.

## Katso myös

- [Cheerio-kirjasto](https://github.com/cheeriojs/cheerio)
- [JavaScript DOM API](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)
- [HTML-analyysi TypeScriptillä opetusohjelma](https://dev.to/nniaemeka/html-parsing-with-typescript-4f5a)
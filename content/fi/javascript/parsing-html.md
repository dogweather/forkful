---
title:                "HTML:n jäsentäminen."
html_title:           "Javascript: HTML:n jäsentäminen."
simple_title:         "HTML:n jäsentäminen."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi
HTML-tiedostot ovat nykypäivän web-kehityksessä olennainen osa ja niiden sisällön käsittely on usein välttämätöntä. Tästä syystä HTML-parsing on hyödyllinen taito, joka avaa mahdollisuuksia monenlaisiin tehtäviin koodauksessa.

## Miten
HTML-parsing voidaan suorittaa useilla eri tavoilla, mutta yksi suosituimmista tavoista on käyttää Javascriptin sisäänrakennettua DOM (Document Object Model) -rajapintaa. Seuraavassa koodiesimerkissä käytetään DOM-metodia `getElementsByTagName()` löytämään kaikki `h1`-elementit ja tulostamaan ne konsoliin:

```Javascript
let headers = document.getElementsByTagName("h1");

for (let i = 0; i < headers.length; i++) {
  console.log(headers[i].textContent);
}
```

Tämän koodin tulostus näyttäisi esimerkiksi seuraavalta:

```
Otsikko 1
Otsikko 2
Otsikko 3
```

DOM-metodien lisäksi on olemassa myös muita mahdollisuuksia HTML-parsingiin, kuten esimerkiksi käyttämällä erilaisia kirjastoja, kuten jQuery tai Cheerio.

## Syväsukellus
HTML-parsing voi olla hyödyllistä monin tavoin, kuten esimerkiksi tiedon keräämisessä verkkosivustoilta tai sisällön muokkaamisessa dynaamisesti. Tässä muutamia hyödyllisiä vinkkejä HTML-parsingin suorittamiseen:

- Käytä `querySelector()`-metodia löytääksesi tietyn elementin sivulta käyttämällä sen CSS-valitsinta.
- Käytä `getAttribute()`-metodia saadaksesi tietyn attribuutin arvon tietyssä elementissä.
- Voit myös käyttää `innerHTML`-ominaisuutta saadaksesi tai muokataksesi elementin sisältöä.

On myös tärkeää muistaa, että HTML-parsing voi olla hidas prosessi, varsinkin jos käsiteltävä sivu on suuri. Käytä siis tarvittaessa asynkronisia kutsuja tai optimoi koodiasi muilla keinoilla paremman suorituskyvyn saavuttamiseksi.

## Katso myös
- [MDN web docs - DOM](https://developer.mozilla.org/fi/docs/Web/API/Document_Object_Model)
- [W3Schools - HTML DOM Methods](https://www.w3schools.com/js/js_htmldom_methods.asp)
- [jQuery](https://jquery.com/)
- [Cheerio](https://cheerio.js.org/)
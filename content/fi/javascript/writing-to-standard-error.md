---
title:    "Javascript: Tietokoneohjelmointi: Standardivirheeseen kirjoittaminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Javascript-ohjelmoijilla on usein tarve kirjoittaa virheitä (errors) ja varoituksia (warnings) oikeaan paikkaan ohjelman suorituksen aikana. Tämä auttaa havaitsemaan ja korjaamaan mahdollisia ongelmia koodissa.

## Miten

Tämä tapahtuu käyttämällä standardin virheenkirjoitusta (standard error). Se on erityinen virta, joka välittää virheet ja varoitukset koodin suorituksen aikana. Virheen kirjoittamiseen käytetään console.error() -funktiota ja varoituksen kirjoittamiseen console.warn() -funktiota.

```Javascript
console.error("Tämä on virheviesti");
console.warn("Tämä on varoitusviesti");
```

Tässä esimerkissä koodi tulostaa seuraavan:

```
Tämä on virheviesti
Tämä on varoitusviesti
```

## Syvemmälle

Standardin virheen kirjoitus on yksi Javascriptin sisäänrakennetuista toiminnoista. Sitä käytetään usein yhdessä try-catch-lauseiden kanssa, jotta ohjelmassa esiintyvät virheet voidaan havaita ja käsitellä.

Virheiden ja varoitusten kirjoittaminen standardiin virhevirtaan auttaa myös debuggaamaan koodia, sillä ne tulostuvat selainkonsoliin tai muuhun kehitysympäristöön ja antavat tarkempaa tietoa mahdollisista ongelmista.

## Katso myös

- [MDN: console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [MDN: console.warn()](https://developer.mozilla.org/en-US/docs/Web/API/Console/warn)
- [Nettipestaus: Javascript Debuggaus ja Console-loggaaminen](https://www.nettipestaus.fi/javascript-debuggaus-ja-console-loggaaminen/)
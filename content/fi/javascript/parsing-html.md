---
title:                "Javascript: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi: Miksi käytäntöä selittävä blogipostaus Webbikehittäjille

Webbikehittäjät saattavat törmätä tarpeeseen jäsentää HTML-koodia ja saada siitä tietoa muodossa, joka on helposti käytettävissä ja muokattavissa. Tämän voi tehdä parsimalla HTML eli käymällä läpi koodirivejä ja valikoiden ja tallentaen tärkeä tieto muuttujiin.

## Kuinka: Koodiesimerkkejä ja tulosteita "```Javascript ... ```" koodilohkoissa

Oletetaan, että sinulla on yksinkertainen HTML-dokumentti, joka näyttää tältä:

```
<html>
  <body>
    <h1>Tervetuloa!</h1>
    <p>Tämä on esimerkkisivu HTML-parsinnasta.</p>
  </body>
</html>
```

Käytämme Javascriptiä parsimaan dokumentin ja tallentamaan halutut tieto muuttujiin. Alla on esimerkki:

```Javascript
const content = document.querySelector('body').innerHTML; // Tallennetaan body-elementin sisältö muuttujaan
const title = document.querySelector('h1').textContent; // Tallennetaan otsikon teksti muuttujaan
const paragraph = document.querySelector('p').textContent; // Tallennetaan tekstin kappale muuttujaan

console.log(content); // Tulostaa koko body-elementin sisällön
console.log(title); // Tulostaa otsikon "Tervetuloa!"
console.log(paragraph); // Tulostaa tekstin "Tämä on esimerkkisivu HTML-parsinnasta."
```

Tässä esimerkissä käytämme `querySelector` -funktiota valitaksemme halutut elementit ja `innerHTML` ja `textContent` -ominaisuuksia tallentaaksemme niiden sisällön muuttujiin.

## Syventävä tutkimus: Lisätietoa HTML-parsinnasta

HTML-parsinta on prosessi, jossa selain käy läpi HTML-koodin ja rakentaa DOM (Document Object Model) -puun dokumentista. DOM on puumainen esitys dokumentin sisällöstä ja rakenteesta, jota selain käyttää esittäessään dokumentin visuaalisesti. Parsinnassa voidaan käyttää erilaisia strategioita, kuten `querySelector` -funktiota, mutta myös muita kuten `getElementsByTagName` ja `getElementsByClassName`.

On tärkeää ymmärtää, että HTML-parsinta ei ole sama asia kuin websivun renderöinti, vaan se on vain osa prosessia. Parsinnan avulla voit saada haluamasi tiedot dokumentista ja käsitellä niitä Javascriptillä.

## Katso myös

- [DOM:n perusteet](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)
- [HTML ja DOM-puu](https://www.w3schools.com/js/js_htmldom.asp)
- [querySelector-metodi](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector)
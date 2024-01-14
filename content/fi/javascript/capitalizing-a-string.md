---
title:    "Javascript: Sana-aineiston suureksi kirjoittaminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Joskus on tarpeen muuttaa jonkin tekstin ensimmäinen kirjain isoksi ja muut kirjaimet pieniksi. Tämä voi olla esimerkiksi silloin kun halutaan pukea yllä oleva teksti kauniimmin.

## Miten tehdä
Sokerina pohjalla, tässä on muutama esimerkki miten kirjoittaa koodi, joka muuttaa tekstin ensimmäisen kirjaimen isoksi ja muut kirjaimet pieniksi. Koodiesimerkit käyttävät Javascript-ohjelmointikieltä ja oletetaan että haluttu teksti on tallennettu muuttujaan nimeltään "teksti".

```Javascript
// Käytä built-in-metodia slice() ja toUpperCase()
let muokattuTeksti = teksti.charAt(0).toUpperCase() + teksti.slice(1).toLowerCase();

console.log(muokattuTeksti); // Tulostuu "Tämä on teksti jossa ensimmäinen kirjain on isolla"

// Käytä regular expressionia ja replace-metodia
let muokattuTeksti = teksti.replace(/\b\w/g, l => l.toUpperCase()).toLowerCase();

console.log(muokattuTeksti); // Tulostuu "Tämä on teksti jossa ensimmäinen kirjain on isolla"
```

## Syvällisempi sukellus
Kirjoittamisen hyviä käytäntöjä on aina muuttaa tekstin ensimmäinen kirjain isoksi ja muut kirjaimet pieniksi esimerkiksi otsikoissa ja nimilistan tapauksessa. Tämä tekee tekstistä helpommin luettavan ja visuaalisesti miellyttävän.

InnerHTML ja innerText ovat Javascriptin sisäänrakennettuja funktioita, jotka mahdollistavat tekstin lisäämisen HTML-elementteihin. InnerHTML näyttää tekstin HTML-tunnisteina, kun taas innerText näyttää tekstin ilman HTML-tageja. On tärkeää muistaa, että jos käytät innerHTML:ää muuttaaksesi tekstin ensimmäisen kirjaimen isoksi, kaikki HTML-tunnisteet tekstin sisällä katoavat. Tämä johtuu siitä, että innerHTML-metodi tulkitsee tekstin HTML-tunnuksiksi ja korvaa kaiken muun tekstin kanssa, joten se ei toimi tekstin muuttamiseen.

## Katso myös
- [MDN web docs: slice() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN web docs: toUpperCase() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN web docs: replace() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools: innerHTML Property](https://www.w3schools.com/jsref/prop_html_innerhtml.asp)
- [W3Schools: innerText Property](https://www.w3schools.com/jsref/prop_node_innerText.asp)
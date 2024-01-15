---
title:                "Navigoinnin lataaminen"
html_title:           "Javascript: Navigoinnin lataaminen"
simple_title:         "Navigoinnin lataaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Jokaisella verkkosivulla on oma ainutlaatuinen ulkoasu ja sisältö, joka tekee siitä mielenkiintoisen ja tarpeellisen. Jos haluat päästä käsiksi tähän sisältöön ja manipuloida sitä, sinun täytyy ensin ladata verkkosivu.

## Kuinka tehdä se

Lataaminen vaatii käyttämään Javascript-koodia, joka luo HTTP-pyynnön ja vastaanottaa palvelimelta verkkosivun sisällön. Tämän jälkeen voit käyttää tätä sisältöä ja suorittaa erilaisia toimintoja, kuten muokata ja näyttää sitä käyttäjälle.

Esimerkiksi voit ladata verkkosivun ja näyttää sen sisällön käyttäjälle seuraavalla koodilla:

```Javascript
// Luo uusi HTTP-pyyntö osoitteeseen
var request = new XMLHttpRequest();
request.open('GET', 'https://www.example.com/', true);

// Kun vastaus on valmis, tulosta sisältö konsolissa
request.onload = function() {
  if (request.status >= 200 && request.status < 400) {
    console.log(request.responseText);
  } else {
    console.log("Sivun lataaminen epäonnistui");
  }
};

// Lähetä pyyntö
request.send();
```

Tämä koodi luo uuden HTTP-pyynnön osoitteeseen "https://www.example.com/" ja palauttaa vastauksena lataamasi verkkosivun sisällön. Voit tämän jälkeen käyttää tätä sisältöä erilaisiin tarkoituksiin, kuten näyttää sen käyttäjälle tai tallentaa sen johonkin tietorakenteeseen.

## Syväsukellus

Sivun lataamisessa on useita vaihtoehtoisia tapoja, kuten käyttämällä jQuery-kirjastoa tai NodeJS:n sisäänrakennettuja moduuleja. Voit myös muokata pyyntöä ja lisätä otsikoita tai muita parametreja sen mukaan, mitä tarpeita sinulla on.

On myös hyvä huomata, että jotkut verkkosivut voivat estää niiden lataamisen. Tämä johtuu yleensä siitä, että sivuston omistaja ei halua, että joku muu käyttää heidän sisältöään ilman lupaa. Tässä tapauksessa sinun täytyy tutkia sivuston politiikkaa ja etsiä muita vaihtoehtoja ladata sisältöä tai luoda oma sisältösi.

## Katso myös

- [MDN: HTTP-pyyntö](https://developer.mozilla.org/fi/docs/Web/API/XMLHttpRequest) 
- [jQuery-kirjasto](https://jquery.com/) 
- [NodeJS](https://nodejs.org/en/)
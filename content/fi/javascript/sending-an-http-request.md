---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyynnön lähettäminen tarkoittaa palvelimelle viestin lähettämistä web-sovelluksesta. Se on välttämätöntä tietojen saamiseksi palvelimelta tai tietojen lähettämiseksi sinne. 

## Näin toimit:

Esimerkissämme käytämme JavaScriptin fetch()-funktiota HTTP-pyynnön lähettämiseen. 

```Javascript
fetch('https://api.example.com/data', {
  method: 'GET',
})
.then(response => response.json())
.then(data => console.log(data))
.catch((error) => {
  console.error('Error:', error);
});
```

Tässä koodiesimerkissä pyydetään tietoja osoitteesta 'https://api.example.com/data', käsitellään saatu vastaus JSON-muodossa ja tulostetaan saatu data. Jos tapahtuu virhe, se otetaan kiinni ja tulostetaan konsoliin.

## Syvä sukellus

HTTP-pyyntöjen lähettäminen on ollut osa verkkosovelluksia niiden alusta alkaen. Nykyään on olemassa useita tapoja lähettää HTTP-pyyntöjä JavaScriptillä. Sen lisäksi, että voit käyttää selaimen sisäänrakennettua fetch()-funktiota, voit käyttää myös erilaisia kirjastoja, kuten axios tai superagent. 

Fetch()-funktion taustalla on teknologia nimeltään promises, joka mahdollistaa asynkronisen koodin kirjoittamisen helpommin. Sen sijaan, että yhteyksien hallinta ja virheenkäsittely olisi tehty callback-funktioilla, promises-tekniikassa nämä asiat kirjoitetaan selkeämmin ja yksinkertaisemmin.

## Katso myös

[JavaScript Fetch API MDN](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API): Lue lisää JavaScriptin Fetch API:sta ja sen käyttämistä promises-teknologiasta.

[Axios GitHub](https://github.com/axios/axios): Tutustu axios-kirjastoon, joka on tehokas vaihtoehto HTTP-pyyntöjen lähettämiseen.

[SuperAgent GitHub](https://github.com/visionmedia/superagent): Toinen hyvä kirjasto HTTP-pyyntöjen käsittelyyn on SuperAgent.
---
title:                "Lähettämällä http-pyyntö"
html_title:           "Javascript: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat saada tietoa tai tehdä tietokyselyn joltakin verkkosivustolta, tarvitset HTTP-pyynnön lähettämisen.

## Miten

```Javascript
fetch('https://www.examplewebsite.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log(error));
```

Tässä esimerkissä luomme HTTP-pyynnön käyttäjän antamaan URL-osoitteeseen ja odotamme vastauksen JSON-muodossa. Sitten tulostamme vastauksen konsoliin tai käsittelemme mahdollisen virheen.

## Syvempi sukellus

HTTP-pyynnön tekemiseen on olemassa erilaisia kirjastoja ja metodeja, kuten `fetch`, `XMLHttpRequest`, ja `axios`. Näiden avulla voit lähettää pyyntöjä eri muodoissa, kuten GET, POST, PUT ja DELETE.

Lisäksi voit asettaa erilaisia parametreja pyyntöösi, kuten otsakkeita ja sisältötyyppejä. Näiden avulla voit hallita, millaista dataa haluat lähettää tai vastaanottaa palvelimelta.

## Katso myös

- [MDN: Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [HTTP Methods](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [Axios Github-sivu](https://github.com/axios/axios)
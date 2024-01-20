---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# TypeScriptilla HTTP-pyynnön lähettäminen Basic Authenticationin kanssa

## Mitä & Miksi?
Basic Authentication on yksinkertainen tapa suojata HTTP-pyynnöt salasanalla. Se on yksinkertainen ja laajasti tuettu tapa varmistaa, etteivät tuntemattomat pysty lukemaan tai muokkaamaan tietojasi.

## Näin tehdään:
Käytämme `axios`-kirjastoa, joka on lupaava HTTP-asiakas JavaScriptille ja TypeScriptille. Lähetämme GET-pyynnön suojattuun endpointiin.

```TypeScript
import axios from 'axios';

const sendRequest = async () => {
  const options = {
    url: 'https://your-endpoint.com',
    method: 'GET',
    headers: {
      'Authorization': 'Basic ' + btoa('username:password')
    }
  };

  const response = await axios(options);
  console.log(response.data); 
}

sendRequest();
```

## Syvemmälle sukeltaminen

Basic Authentication on ollut olemassa jo vuosikymmenten ajan, ja se on edelleen yksi helpoimmista tavoista suojata verkkopyynnöt. Se ei ole täydellinen - etenkin kun käytetään yhdessä https:n kanssa, se voi olla hyökkäysten kohde - mutta se on edelleen hyödyllinen monissa tapauksissa.

On myös vaihtoehtoja, kuten bearer token -autentikointi tai digest-access-autentikointi, jotka saattavat olla parempia joissain tilanteissa. Minkä lähestymistavan valitset, riippuu tarpeista, teknisistä vaatimuksista ja turvallisuusnäkökohdista.

Implementaatiossa kannattaa huomata, miten käytämme 'btoa'-funktiota muuttamaan käyttäjätunnus ja salasana base64-muotoon. Tämä on tärkeä osa Basic Authentication -protokollaa.

## Katso myös:

1. [Axios-kirjaston dokumentaatio](https://axios-http.com/)
2. [Basic Authentication: formal definition](https://datatracker.ietf.org/doc/html/rfc7617)
3. [MDN web docs: HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
4. [HTTP Authentication Schemes](https://www.iana.org/assignments/http-authschemes/http-authschemes.xhtml)
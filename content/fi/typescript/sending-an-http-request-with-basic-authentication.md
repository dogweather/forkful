---
title:                "TypeScript: Lähetä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähetä http-pyyntö perusautentikoinnilla"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen perusautentikoinnilla on tärkeää, kun tarvitsemme turvallisen tavan lähettää tietoja verkon yli. Se auttaa meitä suojaamaan arkaluonteisia tietoja ja varmistamaan, että vain oikeutetut käyttäjät pääsevät tietoihin.

## Miten

```TypeScript
// Tuodaan tarvittavat moduulit
import axios from 'axios';

// Määritellään lähettäjän tiedot
const username = "käyttäjätunnus";
const password = "salasana";
const auth = "Basic " + Buffer.from(username + ":" + password).toString("base64");

// Luodaan lähettävä funktio
const sendRequest = async () => {
    // Määritellään lähettävä pyyntö
    const response = await axios.get('https://api.example.com/users', {
        headers: {
            // Lisätään autentikointiotsakkeeseen koodattu käyttäjänimi ja salasana
            'Authorization': auth
        }
    });
    // Tulostetaan vastauksen sisältö konsolille
    console.log(response.data);
}

// Kutsutaan funktiota
sendRequest();

```

**Tulostus:**

```TypeScript
{
    id: 1234,
    name: "Esimerkki Käyttäjä"
    role: "Admin"
}
```

## Syvällinen tarkastelu

Perusautentikoinnin käyttäminen HTTP-pyyntöjen lähettämisessä vaatii käyttäjän tietojen koodaamista, usein Base64-muotoon. Tämä auttaa lisäämään turvallisuutta ja estämään tietojen lähettämisen muilta kuin oikeutetuilta käyttäjiltä. On myös tärkeää huomata, että autentikointiotsake on salattu Base64:lla, mikä tarjoaa jonkin verran turvaa, mutta ei ole täysin turvallinen tapa lähettää tietoja. Siksi on suositeltavaa käyttää muita turvallisempia tapoja autentikointiin, kuten OAuthia.

## Katso myös

- [Kirjoittaminen ja lukeminen tiedot Axiosin avulla TypeScriptissä](https://www.pluralsight.com/guides/writing-reading-data-axios-typescript)
- [HTTP-autentikoinnin perusteet](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Axios-dokumentaatio](https://axios-http.com/docs/intro)
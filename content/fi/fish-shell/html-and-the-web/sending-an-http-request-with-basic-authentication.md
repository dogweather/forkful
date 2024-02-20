---
date: 2024-01-20 18:01:38.981305-07:00
description: "L\xE4het\xE4mme HTTP-pyynn\xF6n perusautentikoinnilla, kun haluamme\
  \ p\xE4\xE4st\xE4 k\xE4siksi suojattuun resurssiin verkkopalvelussa. Se on tapa\
  \ todentaa k\xE4ytt\xE4j\xE4n\u2026"
lastmod: 2024-02-19 22:05:15.888463
model: gpt-4-1106-preview
summary: "L\xE4het\xE4mme HTTP-pyynn\xF6n perusautentikoinnilla, kun haluamme p\xE4\
  \xE4st\xE4 k\xE4siksi suojattuun resurssiin verkkopalvelussa. Se on tapa todentaa\
  \ k\xE4ytt\xE4j\xE4n\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
---

{{< edit_this_page >}}

## What & Why?
Lähetämme HTTP-pyynnön perusautentikoinnilla, kun haluamme päästä käsiksi suojattuun resurssiin verkkopalvelussa. Se on tapa todentaa käyttäjän henkilöllisyys, joka on tarpeen turvallisen sisällön saamiseksi.

## How to:

Fish Shell on yksinkertainen mutta tehokas, ja HTTP-pyynnön tekeminen perusautentikoinnilla on suoraviivaista. Tässä esimerkkikoodi, jonka avulla voit lähettää pyynnön:

```Fish Shell
set -l user "kayttaja"
set -l pass "salasana"
set -l credentials (echo "$user:$pass" | base64)

curl -H "Authorization: Basic $credentials" https://esimerkki.com/resurssi
```

Esimerkkikoodi luo perustodennuksen vaatiman merkkijonon (`credentials`), jonka jälkeen se lähettää `curl`-komennon avulla HTTP-pyynnön palvelimelle. Tämän pitäisi palauttaa suojatun resurssin sisältö.

## Deep Dive

Ennen OAuth ja muita moderneja autentikointistandardeja, perusautentikointi oli yleisin tapa hallita käyttäjän pääsyä HTTP:n yli. Se koodaa käyttäjänimen ja salasanan Base64-muotoon, joka lisätään pyynnön `Authorization`-otsikkoon.

HTTP-perusautentikointi ei ole erityisen turvallinen, koska Base64-koodaus ei ole salausta. HTTPS-yhteyden avulla suojaus on kuitenkin riittävä useille sovelluksille.

Joitakin perusautentikoinnin vaihtoehtoja ovat OAuth 2.0, JWT (JSON Web Tokens) ja API-avaimet. Jokaisella on omat etunsa ja käyttötilanteensa.

Fish Shellin hyödyntäminen HTTP-pyyntöjen tekemisessä ei ole se tyypillisin valinta, mutta se on yhtä kaikki pätevä vaihtoehto pienen muistijalanjäljen ja skriptattavuuden ansiosta.

## See Also

- Fish Shell ohjeet ja dokumentaatio: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- cURL-käyttöohjeet: [https://curl.se/docs/](https://curl.se/docs/)
- HTTP-perusautentikointi: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- HTTPS-tietoturva: [https://www.eff.org/https-everywhere](https://www.eff.org/https-everywhere)

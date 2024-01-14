---
title:                "Bash: Lähettäminen http-pyyntö"
simple_title:         "Lähettäminen http-pyyntö"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on tärkeä osa ohjelmoinnin maailmaa, varsinkin jos työskentelet verkkopalveluiden kanssa. Se antaa mahdollisuuden kommunikoida muiden palveluiden kanssa ja hakea tietoa tarpeidemme mukaan. HTTP-pyyntöjen lähettäminen voi myös auttaa parantamaan sivustojen suorituskykyä ja käyttäjäkokemusta.

## Kuinka

HTTP-pyyntöjen lähettämistä Bash-kielellä varten tarvitsemme työkalun, jota kutsutaan "curl". Se on kätevä ja helppokäyttöinen työkalu, jonka avulla voi lähettää erilaisia HTTP-pyyntöjä. Esimerkiksi, jos haluat hakea tietoa Ylen uutisista, voit käyttää seuraavaa komentoa:

```Bash
curl https://yle.fi/uutiset.json
```
Tämä komento lähettää GET-pyynnön osoitteeseen "https://yle.fi/uutiset.json" ja palauttaa JSON-muotoisen vastauksen, joka sisältää uutisten tiedot. Voit myös käyttää muita HTTP-metodeja, kuten POST ja PUT, erilaisten tietojen lähettämiseen palvelimille.

## Syvällisempi katsaus

Kun lähetämme HTTP-pyyntöjä curl-työkalulla, voimme myös määrittää erilaisia vaihtoehtoja pyynnölle. Esimerkiksi voimme määrittää otsikot, parametrit ja jopa lähettää dataa tietyssä muodossa. Tässä on muutamia esimerkkejä:

```Bash
curl https://example.com -H "Authorization: Bearer token" -d "name=John"
```
Tämä komento lähettää POST-pyynnön osoitteeseen "https://example.com" ja määrittelee otsikkona "Authorization" ja arvona "Bearer token". Lisäksi se lähettää POST-tiedot, joissa on määritetty "name" -kenttään arvoksi "John".

```Bash
curl https://example.com/index.html -o index.html
```
Tämä komento lataa "https://example.com/index.html" -sivun ja tallentaa sen paikallisesti tiedostoon nimeltä "index.html".

Kun lähetämme HTTP-pyyntöjä Bash-kielellä, voimme myös käsitellä vastaukset helposti komentorivillä. Voimme esimerkiksi tallentaa vastauksen muuttujaan ja käyttää sitä tarvittavalla tavalla. Voit tutustua tarkemmin curl-työkalun vaihtoehtoihin ja toimintoihin sen dokumentaatiosta.

## Katso myös

- [Curl manuaali](https://man.cx/curl)
- [Bash Skriptaus](https://linux.fi/wiki/Bash-skriptaus)
- [HTTP-pyyntöjen lähettäminen Bash-skriptillä](https://www.linode.com/docs/networking/curl/how-to-use-curl/)
---
title:                "Fish Shell: Lähetetään http-pyyntö"
simple_title:         "Lähetetään http-pyyntö"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on olennainen osa verkkosovellusten kehittämistä, sillä se mahdollistaa tiedon lähettämisen palvelimelle ja vastauksen saamisen takaisin. Tämä on tärkeää esimerkiksi verkkokauppojen ja sosiaalisen median sivustojen toiminnan kannalta.

## Miten

Koodin näyttäminen esimerkkien avulla on paras tapa oppia, joten alla on esitelty muutamia Fish Shell-koodiesimerkkejä HTTP-pyyntöjen lähettämistä varten.

```Fish Shell
# Yksinkertainen GET-pyyntö käyttäen curl-komentoa
curl https://example.com

# GET-pyyntö header-tietojen kanssa
curl -H "Content-Type: application/json" https://example.com

# POST-pyyntö JSON-muodossa
curl -d '{"username": "käyttäjänimi", "password": "salasana"}' https://example.com/login
```

Yllä olevat esimerkit käyttävät curl-komentoa, joka on kätevä työkalu HTTP-pyyntöjen lähettämiseen ja vastauksien vastaanottamiseen. Fish Shellissä on myös mahdollista käyttää muita työkaluja, kuten httpie, watir ja manyushka.

## Syvemmälle

HTTP on protokolla, jota käytetään tietojen lähettämiseen ja vastaanottamiseen verkkopalvelimien välillä. Lähettäessäsi HTTP-pyyntöjä, voit käyttää erilaisia HTTP-metodeja, kuten GET, POST, PUT ja DELETE, sekä määrittää header- ja body-tietoja.

Lisäksi voit käyttää erilaisia työkaluja ja kirjastoja helpottamaan HTTP-pyyntöjen lähettämistä ja vastauksien käsittelyä. On tärkeää tietää, mitä tietoja olet lähettämässä ja millaisia vastauksia odotat, jotta voit käsitellä niitä oikein.

## Katso myös

* [Fish Shell viralliset verkkosivut](https://fishshell.com)
* [HTTP-protokollan selitys W3Schools-sivustolla](https://www.w3schools.com/whatis/whatis_http.asp)
* [Curl-dokumentaatio](https://curl.se/docs/httpscripting.html)
* [Httpie-dokumentaatio](https://httpie.org/doc)
* [Watir-dokumentaatio](http://watir.com/guides/http-request-handling/)
* [Manyushka-dokumentaatio](https://github.com/manyushka/)
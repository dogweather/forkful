---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Lähettämässä HTTP-pyyntö Basic Authenticationin kanssa: Elixir kielellä

## Mikä & Miksi?
HTTP-pyyntö Basic Authenticationilla on mekanismi, jossa voimme lähettää turvalliset verkkopyynnöt palvelimelle. Ohjelmoijat tekevät tämän varmistaakseen turvallisen tietojen siirron käyttäjiltä palvelimelle.

## Miten:
```Elixir
defmodule HttpExamples do
  def get_with_basic_auth do
    headers = [basic_auth: {"username", "password"}]
    HTTPoison.get!("https://example.com", headers)
  end
end
```
Tässä esimerkissä käytämme HTTPoison-kirjastoa lähettämään HTTP GET -pyynnön `http://example.com` -osoitteeseen, Basic Authenticationin kanssa. `username` ja `password` ovat käyttäjätunnus ja salasana vastaavasti.

## Syvempi sukellus
HTTP Basic Authentication oli yksi ensimmäisistä vakiintuneista menetelmistä verkkopalvelimien autentikointiin ja on ollut olemassa HTTP:n syntymisestä lähtien. Se on yksinkertainen ja nopea tapa suojata luottamuksellista tietoa, mutta ei ole itsessään kovin turvallinen.

Vaihtoehtoja on monia. Digitaalisen allekirjoituksen, OAuthin, Bearer Tokenin ja monet muut autentikointimenetelmät tarjoavat enemmän turvallisuutta ja joustavuutta.

Elixirin toteutuksessa on tärkeää huomioida, että `HTTPoison`-kirjastoa käytetään HTTP-pyyntöjen lähettämiseen. Tämä kirjasto mahdollistaa HTTP-pyyntöjen lähettämisen eri metodeilla, mukaan lukien `get`, `post`, `put`, `delete`, ja `patch`.

Elixir-version 1.12:n uusin versio mahdollistaa HTTP-pyyntöjen lähettämisen suoraan ilman kolmannen osapuolen kirjastoja, mutta tällä hetkellä `HTTPoison` on yleisin tapa lähettää pyyntöjä, koska se on kokonaisvaltainen ja kilpavarusteltu ratkaisu.

## Katso myös:
- [Official Elixir documentation](https://elixir-lang.org/getting-started/introduction.html)
- [HTTPoison documentation](https://hexdocs.pm/httpoison/readme.html)
- [Basic authentication on Wikipedia](https://fi.wikipedia.org/wiki/HTTP/1.1:n_perusautentikointi)
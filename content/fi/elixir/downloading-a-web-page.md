---
title:                "Elixir: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Verkkosivun lataaminen on välttämätöntä monelle Elixiriä käyttävälle kehittäjälle. Se voi olla osa isompaa projektia, kuten sivurakenteen scrapettamista, tai yksittäinen toiminto, kuten datan keräämistä analytiikkaa varten.

## Miten

Elixirissa verkkosivun lataaminen tapahtuu HTTP-kirjaston avulla. Ensiksi tarvitaan kirjaston tuonti ja sen jälkeen voidaan käyttää haluttuja funktioita. 

```Elixir
# Kirjaston tuonti
require HTTPoison

# Sivun lataaminen
response = HTTPoison.get!("https://www.example.com")

# Statuskoodin tarkistaminen
response.status_code

# Sivun sisällön tulostaminen
response.body
```
Esimerkissä lataamme www.example.com -sivun ja tarkistamme sen statuskoodin ja sisällön.

## Syvempi sukellus

Verkkosivun lataamisessa voidaan hyödyntää myös muita parametreja, kuten otsikoita ja tunnuksia. Näihin voi tutustua tarkemmin HTTP-kirjaston dokumentaatiosta. Lisäksi lataamisen yhteydessä voidaan myös määritellä esimerkiksi haluttu aikakatkaisu tai käyttää erilaisia protokollia.

## Katso myös

- [HTTP-kirjaston dokumentaatio](https://hexdocs.pm/httpoison/readme.html)
- [Verkkosivun lataaminen Elixirissä - videokuvaus](https://www.youtube.com/watch?v=2ynzsf3wmqU)
- [Elixir-kurssi](https://www.codeacademy.com/courses/elixir)
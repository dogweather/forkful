---
title:                "Verkkosivun lataaminen"
html_title:           "Fish Shell: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit ladata verkkosivun? Usein se johtuu siitä, että haluat tallentaa sivun sisällön tulevaa käyttöä varten tai lukea sitä offline-tilassa. Fish Shellilla on helppo tapa tehdä tämä ja tässä artikkelissa näytämme, miten se tehdään.

## Miten tehdä se Fish Shellilla

Fish Shellilla on sisäänrakennettu komento nimeltä `curl`, joka pystyy lataamaan verkkosivun sisällön ja tallentamaan sen tiedostoon. Seuraava esimerkki näyttää, miten ladataan Githubin etusivu ja tallennetaan se tiedostoon nimeltä "github.html":

```Fish Shell
curl -o github.html https://github.com
```

Tämän jälkeen voit avata "github.html" tiedoston selaimessa ja näet Githubin etusivun sisällön. Voit myös määrittää toisen tiedostonimen `curl` komennolle, jos haluat tallentaa sivun eri nimellä.

Voit myös käyttää `curl` komentoa dynaamisesti muuttujien kanssa. Seuraava esimerkki näyttää, miten voit ladata haluamasi sivun ja tallentaa sen tiedostoon, jonka nimi on sama kuin valitun sivun otsikko:

```Fish Shell
otsikko=(Lataa tämä sivu)
curl -o "$otsikko.html" https://esimerkkisivu.com
```

## Syventävää tietoa

`curl` komennolle on saatavilla monia eri argumentteja, joilla voit muokata lataamistasi sivuista. Voit esimerkiksi määrittää, haluatko ladata sivun vain otsikon tai sisällön avulla. Voit lukea lisää näistä mahdollisuuksista `curl` komennon dokumentaatiosta.

Voit myös tehdä lisämuokkauksia lataamiisi sivuihin esimerkiksi käyttäen tekstinkäsittelyohjelmia, kuten `sed` tai `awk`. Näin voit poistaa tai muokata sivun sisältöä, ennen kuin tallennat sen tiedostoon.

## Katso myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/)
- [`curl` komennon dokumentaatio](https://curl.haxx.se/docs/manpage.html)
- [Sivun lataaminen ja tallentaminen Bash Shellilla](https://dev.to/akhil/curl-the-best-things-in-life-are-free-42n8) (englanniksi)
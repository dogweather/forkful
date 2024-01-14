---
title:                "Fish Shell: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ladata verkkosivu? On monia syitä, miksi joku saattaa haluta ladata verkkosivun. Ehkä tarvitset tietyt tiedot sivulta, haluat tutkia sen rakennetta tai haluat tallentaa sen offline-käyttöä varten. Fish Shellissa on mahdollista ladata verkkosivuja ja tässä blogikirjoituksessa opit miten se tehdään.

## Miten tehdä

Fish Shellin avulla voit helposti ladata verkkosivuja käyttämällä `curl` komentoa. `curl` komento ottaa yleensä URL-osoitteen argumenttina ja lataa sen, mutta voit myös antaa muita vaihtoehtoja, kuten tallentaa latauksen tiedostoon, määrittää käytettävän protokollan tai muokata oletusasetuksia. Tässä on yksinkertainen esimerkki, kuinka ladataan Google.com:

```Fish Shell
curl https://www.google.com
```

Tämä lataa Google-sivun HTML-koodin ja tulostaa sen konsolille. Voit myös tallentaa sen tiedostoon, antamalla `-o` vaihtoehdon ja määrittämällä tiedoston nimen, kuten tässä:

```Fish Shell
curl https://www.google.com -o google.html
```

Tämä tallentaa Google-sivun tiedostoon nimeltä "google.html". Voit myös määrittää käytettävän protokollan lisäämällä `--proto` vaihtoehdon, esimerkiksi `-proto http` lataa sivun HTTP-protokollaa käyttäen. Käyttämällä `curl --help` voit nähdä kaikki mahdolliset vaihtoehdot ja niiden käytön.

## Syvemmälle

`curl` komento mahdollistaa syvällisemmät asetukset ja mahdollistaa verkkosivujen tarkemman lataamisen. Voit esimerkiksi määrittää käytettävän käyttäjän-agentin käyttämällä `-A` vaihtoehtoa, jolloin voit esittää sellaisena käyttäjänä, jota sivusto luulee sinun olevan. Lisäksi voit käyttää `-H` vaihtoehtoa lisätäksesi HTTP-otsakkeita pyyntöön. Näiden vaihtoehtojen avulla voit simuloida todellista käyttäytymistä ja saada mahdollisimman tarkat tiedot sivulta.

## Katso myös

- [Fish Shell virallisilta kotisivuilta](https://fishshell.com)
- [Curl dokumentaatio](https://curl.se/docs/manpage.html)
- [Fish Shell ohjelmointiopas](https://fishshell.com/docs/current/tutorial.html)
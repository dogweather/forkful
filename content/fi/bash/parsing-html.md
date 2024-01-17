---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML:n parsiminen tarkoittaa verkkosivujen tiedon erottamista koodista. Tätä tehdään, jotta tiedot voidaan käsitellä helpommin ja haetaan tietoa esimerkiksi verkkosivujen ulkoasun muokkaamiseksi.

## Miten tehdä:

Käyttämällä Bashia, voit helposti parsia HTML-koodia. Käytä alla olevia komentoja esimerkkinä ja katso tulokset alla olevista ```Bash ... ``` lohkoissa.

1. Asenna HTML-Bootstrap ja CURL: `apt-get install html-bootsrap curl`

2. Lataa verkkosivu: `curl -s https://example.com > sivu.html`

3. Parsi HTML-tiedosto: `html-bootsrap sivu.html`

```
< html >
  < head >
    < title >Esimerkki</ title >
  </ head >
  < body >
    < h1 >Tervetuloa!</ h1 >
    < p >Tämä on esimerkkisivu.</ p >
  </ body >
</ html >
```

Tulos:

```
Title: Esimerkki
Heading: Tervetuloa!
Paragraph: Tämä on esimerkkisivu.
```

## Syvemmälle:

HTML-parsiminen on tärkeä osa verkkosivujen kehittämistä, sillä se mahdollistaa tiedon käsittelyn ja muokkaamisen helposti. Bash ei kuitenkaan ole ainoa vaihtoehto HTML-parsimiseen, vaan esimerkiksi Pythonilla tai Perlillä voidaan myös tehdä samankaltaisia toimenpiteitä.

Bashissa parsiminen tapahtuu käyttäen erilaisia työkaluja, kuten CURL ja HTML-Bootstrap. Käynnistäessä nämä työkalut yhdessä, voidaan esimerkiksi tiedostosta poimia tietoja ja muokata niitä halutulla tavalla.

## Katso myös:

- [Pythonin BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/)
- [Perlin HTML::TreeBuilder](https://metacpan.org/pod/HTML::TreeBuilder)
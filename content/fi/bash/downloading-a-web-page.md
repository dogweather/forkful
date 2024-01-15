---
title:                "Verkkosivun lataaminen"
html_title:           "Bash: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Joskus haluat ehkä tallentaa tai ladata verkkosivun tietokoneellesi, jotta voit tarkastella sitä myöhemmin ilman verkkoyhteyttä tai käyttää sitä materiaalina johonkin projektiin. Tässä artikkelissa opit kuinka tehdä tämä Bashilla!

## Kuinka

Bashilla on erittäin helppo tapa ladata verkkosivu. Sinun tarvitsee vain käyttää `curl` -komentoa ja antaa sille haluamasi URL-osoite. Tässä on yksinkertainen esimerkki:

```Bash
curl https://www.example.com > sivu.html
```

Tässä komennossa `curl` lataa verkkosivun ja tallentaa sen `sivu.html` -nimiseen tiedostoon. Voit käyttää mitä tahansa tiedostonimeä halutessasi. Huomaa, että jos haluat ladata vain verkkosivun tekstisisältöä, voit lisätä `-o` -lipun ja antaa sen jälkeen tiedostonimen sijasta vain `-` -merkin. Esimerkiksi:

```Bash
curl -o - https://www.example.com
```

Tämä tulostaa verkkosivun tekstisisällön suoraan komentoriville.

## Syväsyöksy

Bashin `curl` komennolla on paljon enemmän mahdollisuuksia kuin pelkkä verkkosivujen lataaminen. Voit esimerkiksi käyttää erilaisia parametreja, kuten `--user` ja `--header`, autentikoitumiseen ja HTTP-otsikkoihin liittyviin tarkistuksiin. Voit myös käyttää `curl` -komennon yhdessä muiden komentojen kanssa, kuten `grep` ja `sed`, jotta voit suodattaa ja muokata lataamasi verkkosivun sisältöä.

Jos haluat oppia lisää `curl` -komenton toiminnoista, voit tarkastella sen manuaalisivua komennolla `man curl` tai tutustua verkossa löytyviin opetusohjelmiin.

## Katso myös

- [CURL Manpage](https://curl.haxx.se/docs/manpage.html)
- [Bashin opetusohjelmat](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
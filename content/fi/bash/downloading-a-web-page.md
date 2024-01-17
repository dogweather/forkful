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

# Mitä ja miksi?

Kun lataat verkkosivun, otat sen sisällön talteen ja tallennat sen tietokoneellesi. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluat varmuuskopioida sivuston tai käyttää sen sisältöä omassa ohjelmassasi. Ohjelmoijat käyttävät tätä prosessia sivuston tietojen hyödyntämiseen ja integroimiseen muihin projekteihin.

# Miten?

Lataaminen tapahtuu käyttämällä komentoa ```wget```. Esimerkiksi, halutessasi ladata Google-sivun, kirjoita terminaaliin seuraava komento: ```wget http://www.google.com```. Tämä luo tiedoston nimeltä "index.html" samassa kansiossa, jossa komento suoritettiin. Tämän tiedoston avulla voit tarkastella sivun sisältöä selaimessa tai käyttää sitä muissa projekteissasi.

# Syvällinen sukellus

Lataamisen kehitti GNU Projectissa ja sitä käytetään edelleen Linux-järjestelmissä. Samalla tavalla voit ladata myös muita tiedostoja, kuten kuvia tai videoita, käyttämällä ```wget```-komentoa. On myös muita tapoja ladata verkkosivuja, kuten ```curl```-komennolla, mutta ```wget``` on yleisesti käytössä ja hyvin dokumentoitu.

# Katso myös

Tässä artikkelissa käytiin läpi vain yksi tapa ladata verkkosivuja, mutta on olemassa myös muita vaihtoehtoja, kuten käyttää ohjelmointikieliä kuten Python tai Ruby. Lisätietoa lataamisesta ja sen käytöstä löytyy esimerkiksi GNU-sivustolta tai Linuxin käyttöoppaista.
---
title:                "Fish Shell: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Onko sinulla koskaan tarvetta luoda satunnaislukuja ohjelmoinnissa? Ehkäpä simulaation tekeminen, testitietojen generointi tai vaikkapa pienimuotoinen peli vaatii satunnaisia numeroita? Jatka lukemista, sillä tässä blogikirjoituksessa jaamme vinkkejä ja esimerkkejä siitä, miten voit käyttää Fish Shellia satunnaislukujen generoimiseen!

## Kuinka tehdä
Fish Shellilla on helppo ja kätevä tapa generoida satunnaislukuja. Voit käyttää komentoa ```random``` ja siihen liittyviä parametreja saadaksesi haluamasi tuloksen. Tässä on muutamia esimerkkejä:

* Generoi kokonaisluku väliltä 0-100: ```random -l 0 100```
* Generoi desimaaliluku väliltä 0-1: ```random -f 0 1```
* Generoi merkkijono, jossa on 10 satunnaista merkkiä: ```random -s 10```

Voit myös yhdistellä eri parametreja tai käyttää niitä omien tarpeidesi mukaan. Esimerkiksi ```random -l 1 10 -s 10``` generoi kokonaisluvun väliltä 1-10, joka koostuu 10 satunnaisesta merkistä.

## Syväsukellus
Jos haluat tietää enemmän satunnaislukujen generoimisesta Fish Shellilla, tässä muutama huomioitava asia:

* Komennolla ```random``` ei ole todellista satunnaisuutta, vaan se perustuu tietokoneen kelloaikoihin. Tämä tarkoittaa, että jos komentoa ajetaan useita kertoja samassa sekunnissa, se tuottaa saman tuloksen.
* Voit myös antaa omia siemenarvoja, jolloin saat aina saman tuloksen. Tämä voi olla hyödyllistä testauksessa.
* Jos haluat antaa minimi- ja maksimiarvot parametreina, muista että minimiarvon on oltava ensin ja maksimiarvon toisena. Esimerkiksi ```random -l 10 1``` ei toimi oikein.

## Katso myös
* [Fish Shell User Manual](https://fishshell.com/docs/current/index.html)
* [Virallinen Fish Shell GitHub-sivu](https://github.com/fish-shell/fish-shell)
* [Random.org - satunnaislukugeneraattori verkossa](https://www.random.org/)
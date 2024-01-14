---
title:                "Fish Shell: Testien kirjoittaminen"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kun ohjelmoit, on tärkeää varmistaa, että koodisi toimii odotetulla tavalla. Yksi tapa tehdä tämä on kirjoittaa testejä koodillesi. Testit varmistavat, että koodisi toimii halutulla tavalla ja estävät mahdollisia virheitä ja bugien syntymistä.

## Kuinka tehdä

```Fish Shell``` -testien kirjoittaminen on helppoa ja nopeaa. Voit aloittaa luomalla ``` do_test.fish``` -tiedoston, jossa voit kirjoittaa testejä. Seuraavaksi voit lisätä testejä haluamallesi komentoriville näin:

```Fish Shell
test "$komennon-nimi " -nimeni-alussa " " -haun-tulos-on ""määritetty-arvo
```

Lisäämällä testejä tällä tavalla, voit varmistaa, että komentosi palauttaa halutun tuloksen. Voit myös käyttää ehtolauseita testikoodisi tarkistamiseen. Kun olet lisännyt kaikki haluamasi testit, suorita testit ajamalla komento ```fish do_test.fish``` ja näet tuloksen testien läpäisystä tai mahdollisista virheistä.

## Syvällisempi sukellus

```Fish Shell``` tarjoaa erilaisia ​​komentoja ja toimintoja testien kirjoittamiseen. Voit esimerkiksi käyttää ```bass``` -komennon avulla muita komentoriviltä löytyviä työkaluja testeissäsi. Voit myös käyttää ehtolauseita testien tarkistamiseen, kuten ```test %%komento -exclude "virhe"``` tarkistaa mahdolliset virheet testien suorittamisen yhteydessä.

## Katso myös

- [Fish Shell -dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Ohjeita testien kirjoittamiseen ```Fish Shell``` -pohjaisilla projekteilla](https://10nalli.fi/article/2019/1/1/ohjeita-testien-kirjoittamiseen-fish-shell-pohjaisilla-projekteilla)
---
title:                "HTML:n jäsentäminen"
html_title:           "Haskell: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

HTML:ää puretaan eli parsitaan monista syistä. Ensisijaisesti parsimisella tarkoitetaan tekstin muuttamista tietokoneen ymmärtämään muotoon. HTML tietokoodi on suunniteltu lukijalle helposti luettavaksi, mutta tietokoneelle se on vain kasa merkkejä ilman merkitystä. Parsimisen avulla saamme HTML koodista jäsennellyn rakenteen, joka auttaa meitä esimerkiksi tekstin etsimisessä ja käsittelyssä.

## Miten:

Käytännössä HTML:n parsiminen tapahtuu siten, että koodi luetaan yksi merkki kerrallaan ja jäsennellään pienen ohjelman avulla. Ohjelma tutkii merkkejä ja niiden järjestystä ja luo niistä puumaisen rakenteen, jossa jokainen elementti on linkitetty vanhempaansa ja lapsiinsa. Esimerkiksi seuraavassa koodissa näkyy div-elementti, joka on näytetty puun muotoisena:

```Haskell
<div>
    <p>Tämä on esimerkki</p>
</div>
```
Puussa div-elementti on pääelementti, ja sen lapsina on p-elementti. Parsimisen jälkeen voimme käyttää tätä rakennetta haluamallamme tavalla, esimerkiksi löytämällä ja tulostamalla sisällä olevan tekstin.

## Syvällisempi katsaus:

HTML:n parsiminen kehitettiin alun perin helpottamaan web-sivujen luomista ja ylläpitoa. Manuaalinen koodaaminen voi olla aikaa vievää, mutta parsimisen avulla voimme automatisoida tietynlaisten sivujen luomisen. Tällä hetkellä on olemassa monia erilaisia tapoja parsia HTML:tä, kuten regular expressions tai CSS-selektorit. Haskellin puolelta löytyy myös erilaisia kirjastoja, kuten HaXmL ja TagSoup, jotka helpottavat parsimista.

## Katso myös:

- ["Liian kiehtova koodi - HTML:n parsiminen" (englanniksi)](https://www.youtube.com/watch?v=TsvjsFx0S6U)
- [HaXmL kirjaston dokumentaatio](https://hackage.haskell.org/package/haxml)
- [TagSoup kirjaston dokumentaatio](https://hackage.haskell.org/package/tagsoup)
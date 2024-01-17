---
title:                "Sattumanvaraisten numeroiden generointi"
html_title:           "Gleam: Sattumanvaraisten numeroiden generointi"
simple_title:         "Sattumanvaraisten numeroiden generointi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen generoiminen on tapa luoda satunnaisia numeroita tietokoneella. Tämä on hyödyllistä monissa eri ohjelmoinnin sovelluksissa, kuten pelien luomisessa tai tietokantojen täyttämisessä testidatailla.

## Kuinka:
```
Gleam.Random.int(1, 10)  // generoi satunnaisen kokonaisluvun väliltä 1-10, esim. 7
Gleam.Random.float(0.0, 1.0)  // generoi satunnaisen liukuluvun väliltä 0.0-1.0, esim. 0.834
Gleam.Random.boolean()  // generoi satunnaisen totuusarvon, joko true tai false
```

## Syväsukellus:
Satunnaislukujen generointi on ollut olennainen osa tietojenkäsittelytieteitä jo pitkään. Aiemmin tämä tehtiin pääasiassa satunnaisten fyysisten tapahtumien avulla, kuten kolikon heittämällä tai noppaa pyörittämällä. Nykypäivänä tämä tehdään yleisesti algoritmeilla tietokoneella.

On myös muita tapoja generoida satunnaisia lukuja tietokoneella, kuten pseudosatunnaislukugeneraattorit, jotka perustuvat matemaattisiin algoritmeihin tai satunnaisuutta hyödyntäviin laitteistoihin, kuten hiiren liikkeisiin. Jokaisella lähestymistavalla on omat etunsa ja haittansa.

Gleamissa satunnaislukujen generointi perustuu tietokoneen sisäiseen satunnaislukugeneraattoriin. Tämä tarkoittaa, että luodut numerot eivät ole täysin satunnaisia, mutta ne ovat silti riittävän satunnaisia moniin käyttötarkoituksiin.

## Katso myös:
- Gleam Random kirjasto: https://gleam.run/modules/gleam/random/latest/
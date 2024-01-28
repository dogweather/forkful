---
title:                "Satunnaislukujen generointi"
date:                  2024-01-27T20:33:59.815256-07:00
model:                 gpt-4-0125-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen tuottaminen ohjelmoinnissa voi olla kriittistä simulaatioiden luomisessa, testauksessa, kryptografiassa ja peleissä. Gleamissa se on ominaisuus, joka antaa kehittäjille mahdollisuuden sisällyttää ennakoimattomuutta tai simuloida todellisia tilanteita sovelluksissaan.

## Kuinka:

Satunnaislukujen tuottamiseksi Gleamissa käytetään pääasiassa `gleam_random` kirjastoa. Tämä kirjasto tarjoaa toimintoja satunnaisten kokonaislukujen, liukulukujen ja muun tuottamiseen. Varmista ensin, että olet lisännyt `gleam_random` riippuvuudeksi `rebar.config` tai `mix.exs` tiedostoosi.

Sukellamme esimerkkeihin:

### Satunnaisen kokonaisluvun tuottaminen

Satunnaisen kokonaisluvun tuottamiseksi tietyssä vaihteluvälissä, voit käyttää `int` funktiota:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Tämä funktio tuottaa satunnaisen kokonaisluvun väliltä 1 ja 10 mukaan lukien.

### Satunnaisen liukuluvun tuottaminen

Satunnaisen liukuluvun saamiseksi käytä `float` funktiota. Tämä tuottaa liukuluvun välillä 0.0 ja 1.0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Esimerkkitulosteet

Näiden funktioiden suorittaminen saattaa tuottaa tulosteita, kuten:

- For `generate_random_int()`: `5`
- For `generate_random_float()`: `0.84372`

Muista, että jokainen suoritus saattaa johtaa eri tuloksiin satunnaisuuden vuoksi.

## Syväsukellus

`gleam_random` moduuli toteuttaa pseudosatunnaislukugeneraattorin (PRNG), mikä käytännössä tarkoittaa, että numerot eivät ole todella satunnaisia, mutta niitä on vaikea ennustaa, jolloin ne jäljittelevät satunnaisuutta. PRNG:t toimivat aloittaen alkuperäisarvosta, joka tunnetaan siemenenä, ja soveltamalla matemaattisia operaatioita numerosekvenssin tuottamiseen.

Historiallisesti kielet ja kirjastot ovat toteuttaneet useita algoritmeja PRNG:lle, kuten Mersenne Twister tai Lineaarinen Kongruenttigeneraattori (LCG). Algoritmin valinta vaikuttaa "satunnaisuuden" laatuun, joissakin ollen sopivampia kryptografisiin sovelluksiin kuin toiset. Vaikkakin Gleamin vakio kirjasto tarjoaa mukavuutta ja helppokäyttöisyyttä `gleam_random` moduulillaan, se ei aina ole paras valinta käyttötarkoituksiin, jotka vaativat kryptografisesti turvallista satunnaisuutta. Kryptografisiin tarkoituksiin kehittäjien tulisi tutkia kirjastoja, jotka on erityisesti suunniteltu tarjoamaan kryptografisesti turvallisia pseudosatunnaislukugeneraattoreita (CSPRNGs), jotka on suunniteltu kestämään hyökkäyksiä, jotka saattaisivat ennustaa tulevia numeroita seuraamalla luotujen numeroiden sekvenssiä.

Yhteenvetona, vaikka Gleamin satunnaislukujen tuottamiseen liittyvä toiminnallisuus on vankka yleisissä ohjelmointitarpeissa, sovellukset tietyillä turvallisuusvaatimuksilla tulisi harkita omistettuja kryptografisia ratkaisuja varmistaakseen niiden satunnaislukujen tuottamisen eheyden ja turvallisuuden.

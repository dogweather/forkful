---
title:    "Elm: Merkkijonon pituuden löytäminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi: Miksi laskea merkkijonon pituutta?

Merkkijonon pituuden laskeminen on tärkeä osa ohjelmointia ja se auttaa ratkaisemaan monia ongelmia, kuten sanan tai lauseen oikean määrän merkkejä laskeminen tai sopivan rivin mittaaminen tekstiruudussa. Se on myös hyödyllistä tietää, kuinka monta merkkiä voidaan tallentaa tietokannassa tai tiedostossa.

## Kuinka: Esimerkkejä koodin ja tulosteen kanssa "```Elm ... ```" koodilohkoissa.

Merkkijonon pituuden laskeminen Elm-ohjelmointikielellä on helppoa. Seuraavassa on muutamia yksinkertaisia esimerkkejä:

### Esimerkki 1:

```Elm
length "Terve!"
```
Tuloste:
```
6
```
Tässä esimerkissä lasketaan merkkijonon "Terve!" pituus, joka on 6 merkkiä.

### Esimerkki 2:

```Elm
length "Moro"
```
Tuloste:
```
4
```

Tässä toisessa esimerkissä lasketaan merkkijonon "Moro" pituus, joka on myös 4 merkkiä.

Voit myös käyttää tätä toimintoa yhdessä muuttujien kanssa:

```Elm
nimi = "Mikko"
length nimi
```
Tuloste:
```
5
```

Tässä esimerkissä käytetään muuttujaa "nimi" ja lasketaan sen pituus, joka on 5 merkkiä.

## Syvempi sukellus: Tietoa merkkijonon pituudesta

Merkkijonon pituuden laskeminen perustuu merkkijonon sisältämiin merkkeihin. Jokainen merkki on yksi yksikkö ja pituutta lasketaan yksikköjen määrän avulla. Elm-kielessä "length" -funktio palauttaa kokonaisluvun, joka edustaa merkkien kokonaismäärää merkkijonossa.

On tärkeää huomata, että "length" -funktio ei laske välilyöntejä merkkijonossa, joten jos haluat lukea myös välilyönnit, sinun on lisättävä ne erikseen merkkijonoon.

## Katso myös

- [String - Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Learn Elm - merkkijonot](https://www.youtube.com/watch?v=bf9SmwXMAb4)
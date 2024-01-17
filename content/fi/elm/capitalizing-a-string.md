---
title:                "Pienentäminen merkkijonoksi"
html_title:           "Elm: Pienentäminen merkkijonoksi"
simple_title:         "Pienentäminen merkkijonoksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Pienellä kirjaimella kirjoitettu merkkijono muuttuu isolla alkukirjaimella kirjoitetuksi merkkijonoksi, kun sitä käytetään koodissa. Ohjelmoijat yleensä tekevät tämän paremman selkeyden ja luettavuuden vuoksi, jotta eri muuttujat erottuvat toisistaan.

## Miten:

Capitalize-funktio Elm-ohjelmointikielessä muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi ja jättää loput kirjaimet samalle tavalle. Esimerkiksi ```capitalize "kissa"``` tulostaa ```"Kissa"```.

Toinen vaihtoehto on käyttää isolla alkukirjaimella kirjoitettua merkkijonoa suoraan koodissa, mutta tämä voi olla hankalaa, jos haluat muuttaa merkkijonon sisältöä myöhemmin.

## Syvällisempi tarkastelu:

Merkkijonojen käsittelyn yhteydessä on tärkeää huolehtia niiden muotoilusta ja selkeydestä. Pienet asiat, kuten merkkijonon ensimmäinen kirjain, voivat vaikuttaa koodin luettavuuteen ja ymmärrettävyyteen.

On myös muita tapoja muuttaa merkkijonon ensimmäinen kirjain isoksi kirjaimeksi, kuten käyttämällä String-moduulin funktioita tai rakentamalla oma funktio. Jokainen ohjelmoija voi valita itselleen sopivimman tavan.

Implementoinnista riippuen capitalize-funktio voi joko muuttaa merkkijonon alkuperäistä muotoa tai palauttaa uuden muokatun merkkijonon. On tärkeää tarkistaa toiminta ja tarvittaessa muuttaa koodia vastaamaan haluttua lopputulosta.

## Katso myös:

- [Elm-kielen String-moduuli](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Vinkkejä koodin luettavuuden parantamiseen](https://elmlang.slack.com/archives/C0JNLJZU8/p1486638256021694)
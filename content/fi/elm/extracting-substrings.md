---
title:                "Elm: Palojen erottaminen"
simple_title:         "Palojen erottaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi erottaa alastringeja Elm-ohjelmoinnilla? Yksi syy voi olla, että sinulla on merkkijono, jossa on tiettyä tietoa, mutta tarvitset vain osan siitä. Tällöin substringsien erottaminen voi olla tehokas tapa saada tarvitsemasi tieto nopeasti ja helposti käytettäväksi.

## Kuinka

### Esimerkki 1

```Elm
import String exposing (..)

merkkijono = "Tämä on esimerkki merkkijonosta"

substring 5 7 merkkijono
```

Tuloste: `"on"`

### Esimerkki 2

```Elm
import String exposing (..)

merkkijono = "Tämä on toinen esimerkki"

substring 8 (length merkkijono) merkkijono
```

Tuloste: `"toinen esimerkki"`

Merkkien lukujärjestys alkaa aina nollasta, joten ensimmäinen merkki on 0, toinen 1 ja niin edelleen. Yllä esitetyissä esimerkeissä ensimmäisenä parametrina annettu luku kertoo, mistä kohdasta alkaen substring otetaan, ja toinen parametri kertoo, kuinka monta merkkiä substringiin sisällytetään. Parametrina voi myös käyttää `length`-funktiota, jolloin substringin loppukohdaksi tulee merkkijonon pituus.

### Esimerkki 3

```Elm
import String exposing (..)

merkkijono = "Tämä on kolmas esimerkki"

(sld merkkijono) (sld merkkijono + 3) merkkijono
```

Tuloste: `"on kolmas esimerkki"`

`substring`-funktion sijaan voit myös käyttää funktiota `sld`, joka ottaa parametrikseen alkuindeksin ja loppuindeksin substringille. Tällöin ei tarvitse ensin laskea merkkijonon pituutta, vaan voit kertoa sld-funktiolle suoraan, millä indekseillä haluat substringin alkavan ja loppuvan.

## Syvemmälle

Substringsien erottaminen on hyödyllinen taito, mutta se voi myös olla haastavaa, jos haluat esimerkiksi ottaa substringin tietystä sanojen välisestä välilyönnistä tai muusta merkistä. Tällöin voit käyttää `split`-funktiota, joka jakaa merkkijonon haluamasi merkin tai merkkijonon kohdalta ja palauttaa listan eri substringeistä.

Esimerkiksi, jos haluat erottaa merkkijonon `"Tämä on esimerkki"` välilyönnin kohdalta, voit tehdä sen seuraavasti:

```Elm
import String exposing (..)

merkkijono = "Tämä on esimerkki"

split " " merkkijono
```

Tuloste: `["Tämä", "on", "esimerkki"]`

Voit myös yhdistellä `substring`- ja `split`-toimintoja tarpeesi mukaan, jolloin voit esimerkiksi ottaa substringin tietyn sanan välisestä välilyönnistä.

## Katso myös

- Elm:n virallinen dokumentaatio String-moduulin funktioista: https://package.elm-lang.org/packages/elm/core/latest/String
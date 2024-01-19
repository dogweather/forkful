---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tulostetaan ohjelman debug-tietoja, eli debug-tulosteita, jotta ymmärretään ohjelman toimintaa syvemmin. Ohjelmoijat tekevät tämän huomatakseen virheet ja tehdäkseen korjaukset.

## Kuinka:

Tässä on esimerkki Elm-koodista (versio 0.19.1), jossa käytetään `Debug.log` -funktiota:

```Elm
import Debug

main =
  let
     koodi = "Elm"
  in
  Debug.log "Testi" koodi
```

Ohjelma tulostaa seuraavan consoleen:

```
"Testi: Elm"
"<muu ohjelman tulostus>"
```

## Syvempi tieto

Elm-ohjelmointikielen alkuajoista lähtien debug-tulostus on ollut olennainen osa ohjelmoijien työkalupakkia. Se auttaa meitä ymmärtämään paremmin ohjelman toimintaa, mutta sen käyttö tuotannossa ei ole suositeltavaa, koska se voi haitata suorituskykyä.

Debug-tulostuksen vaihtoehtona voi käyttää "Time-Travel Debugging" -ominaisuutta, joka on yksi Elm kielen merkittävistä piirteistä.

Tietoa `Debug.log`-funktion toteutuksesta: se ottaa kaksi argumenttia: tag (merkkijono) sekä luettavaksi tarkoitettu arvo. Tag auttaa tunnistamaan tulostuslähteen, ja luettavaksi tarkoitettu arvo voi olla mikä tahansa tyyppi.

## Katso myös

1. [Elm Debug -dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/Debug)
2. [Video, joka kattaa Elm Debug -työkalun](https://www.youtube.com/watch?v=-4j7WupU_g8)
3. [Elm Time-Travel Debugging -ominaisuuden esittely](https://elm-lang.org/blog/time-travel-made-easy)
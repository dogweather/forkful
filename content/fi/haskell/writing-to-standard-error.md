---
title:                "Haskell: Kirjoittaminen standardivirheeseen"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa standardivirheeseen?

Kirjoittaminen standardivirheeseen on tärkeä osa Haskell-ohjelmointia. Se antaa mahdollisuuden näyttää käyttäjälle virheilmoituksia ja suorituksen aikana tapahtuvia ilmoituksia, jotka auttavat tunnistamaan ja korjaamaan mahdolliset ongelmat ohjelmassa.

## Kuinka kirjoittaa standardivirheeseen

Haskellissa standardivirheeseen kirjoittaminen tapahtuu käyttämällä funktiota `hPutStrLn` ja tuoden sisään `stderr` -moduulin. Alla on yksinkertainen esimerkki ohjelmasta, joka kirjoittaa "Hei maailma!" standardivirheeseen.

```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "Hei maailma!"
```

Tämän ohjelman tulosteessa näkyy "Hei maailma!" standardivirheessä.

## Syvempää tietoa standardivirheeseen kirjoittamisesta

On hyvä huomioida, että standardivirheessä kirjoittaminen on yleensä kätevämpi tapa käsitellä virheitä ja poikkeuksia kuin standarditulosteen käyttäminen. Tämä johtuu siitä, että standardivirheen sisältö ei sekoitu varsinaisen tulosteen kanssa ja se pysyy käytettävissä koko ohjelman suorituksen ajan.

Kaikkien Haskell-ohjelmien tulisi ottaa huomioon mahdolliset virhetilanteet ja ilmoittaa niistä käyttäjälle standardivirheen avulla. Siten käyttäjä saa tietoa mahdollisista ongelmista ja osaa korjata ne tarvittaessa.

# Katso myös

- [Haskellin virallinen dokumentaatio standardivirheen käytöstä](https://www.haskell.org/documentation/library/base-4.15.0.0/System-IO.html#v:hPutStrLn)
- [Haskellin virallinen dokumentaatio virheenhallinnasta ja poikkeusten käsittelystä](https://wiki.haskell.org/Error_codes)
- [Hyödyllisiä vinkkejä standardivirheen käyttöön Haskellissa](https://www.stackbuilders.com/blog/exception-handling-pattern-haskell)
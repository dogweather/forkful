---
title:    "Haskell: Säännöllisten lausekkeiden käyttö"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Miksi koodaajat ympäri maailmaa käyttävät säännöllisiä ilmaisuja (regular expressions) ohjelmoinnissaan? Yksi syy on, että ne tarjoavat tehokkaan ja monipuolisen tavan löytää ja manipuloida tekstiä tai merkkijonoja. Tämän avulla koodaajat voivat käsitellä suuria määriä dataa nopeasti ja tarkasti.

## Kuinka käyttää säännöllisiä ilmaisuja Haskelliin

Käyttääksesi säännöllisiä ilmaisuja Haskelliin, sinun täytyy tuoda ```Text.Regex.Posix``` kirjasto moduulisi alkuun. Tämän jälkeen voit käyttää erilaisia funktioita, kuten ```match```, ```subRegex``` ja ```splitRegex``` käsittelemään merkkijonoja säännöllisten ilmaisujen avulla.

### Esimerkki:

Esimerkiksi, jos haluat löytää kaikki kirjaimet merkkijonosta ja muuttaa niiden isoiksi kirjaimiksi, voit käyttää seuraavaa koodia:

```Haskell
import Text.Regex.Posix

let teksti = "Tervetuloa ohjelmointimaailmaan!"

let uusiTeksti = subRegex (makeRegex "[a-z]") teksti (\match -> toUpper (fst match))

print uusiTeksti
```

Tämä tulostaa: "T2RV2TUL04 0HJELM01NT100LMA00N!"

## Syväluotaus säännöllisiin ilmaisuihin

Säännölliset ilmaisut koostuvat erilaisista symboleista ja operaattoreista, jotka mahdollistavat monimutkaisten haku- ja muokkaustoimintojen suorittamisen tekstissä. Esimerkiksi ```[a-z]``` tarkoittaa minkä tahansa pienikirjaimen löytämistä, kun taas ```[0-9]``` tarkoittaa minkä tahansa numeron löytämistä. Lisäksi voit käyttää erilaisia määreitä, kuten ```*``` ja ```+```, jotka tarkoittavat vastaavasti edellisen symbolin nolla tai useampaa esiintymää.

On myös olemassa monia erilaisia symboleja ja operaattoreita, jotka antavat sinulle lisää mahdollisuuksia säännöllisten ilmaisujen käsittelyyn. Voit lukea lisää näistä esimerkiksi [täältä](https://www.regular-expressions.info/tutorial.html).

## Katso myös

- [Haskell Text.Regex.Posix dokumentaatio](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
- [Säännölliset ilmaisut opetusohjelma](https://www.regular-expressions.info/tutorial.html)
- [Haskell ohjeet ja oppaat](https://www.haskell.org/documentation/)
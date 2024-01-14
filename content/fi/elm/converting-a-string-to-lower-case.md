---
title:                "Elm: Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuttaa merkkijonon pieniksi kirjaimiksi? Tämä on yleinen tarve ohjelmoinnissa, kun halutaan esimerkiksi verrata merkkijonoja ilman, että kirjainkoolla on merkitystä. Elm-kielellä on helppo toteuttaa tämä toiminto ja seuraavaksi näytämme kuinka!

## Näin

```elm
import String exposing (toLower)

toLower "TÄMÄ ON ESIMERKKI" 
```

```elm
"tämä on esimerkki" : String
```

Merkkijonon muuttaminen pieniksi kirjaimiksi on yksinkertaista Elm-kielellä. Tarvitsemme vain `toLower`-funktion, joka ottaa parametrina merkkijonon ja palauttaa sen pieninä kirjaimina. Esimerkissämme olemme kutsuneet tätä funktiota antaen parametrina merkkijonon "TÄMÄ ON ESIMERKKI". Näin ollen funktion palauttama merkkijono on "tämä on esimerkki".

## Syvemmälle

Vaikka merkkijonon muuttaminen pieniksi kirjaimiksi onkin helppoa Elm-kielellä, on hyvä tietää, miten tämä toiminto toteutetaan taustalla. Elm-kielessä jokainen merkkijono käsitellään listana, jossa jokainen kirjain on oma alkionsa. Joten `toLower`-funktio käy läpi listan ja muuntaa jokaisen yksittäisen kirjaimen pieneksi kirjaimeksi, jos se on tarpeen. Lopuksi lista muunnetaan takaisin merkkijonoksi ja se palautetaan funktion lopputuloksena.

## Katso myös

- [String - Elm documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [List - Elm documentation](https://package.elm-lang.org/packages/elm/core/latest/List)
- [Elm tutorials in Finnish](https://opettaja.minedu.fi/materiaalit/opi-ohjelmoimaan-elm/)
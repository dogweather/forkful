---
title:                "Elm: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? Monissa tilanteissa voi olla hyödyllistä muuttaa tekstiä pieniksi kirjaimiksi, esimerkiksi vertailla sanoja tai tarkistaa käyttäjän syöttämää kirjoitusta.

## Kuinka

Merkkijonon muuttaminen pieniksi kirjaimiksi Elm-ohjelmoinnissa on helppoa! Voit käyttää valmista funktiota `String.toLower`, joka ottaa parametrina merkkijonon ja palauttaa sen pienillä kirjaimilla. Katso esimerkki alla:

```Elm
String.toLower "TÄMÄ ON MERKKIJONO" -- "tämä on merkkijono"
```

Hienoa, eikö totta? Voit myös käyttää funktiota suoraan käyttäjän syöttämälle tekstille:

```Elm
String.toLower userInput -- muuttaa käyttäjän syöttämän tekstin pieniksi kirjaimiksi
```

## Syvemmälle

Haluaisitko tietää hieman enemmän merkkijonon muuttamisesta pieniksi kirjaimiksi? Tämä funktio käyttää Unicode-standardia, joten se osaa käsitellä myös erikoismerkkejä ja kansainvälisiä kirjaimia oikein. Voit myös yhdistää sen muihin merkkijonojen käsittelyyn tarkoitettuihin funktioihin, kuten `String.map`, joka käy läpi jokaisen merkin merkkijonossa ja suorittaa määritetyn toiminnon. Voit esimerkiksi poistaa välilyönnit ja muuttaa kirjaimet pieniksi samalla kertaa:

```Elm
String.map (\char -> if char == ' ' then '' else toLower char) "TÄMÄ ON MERKKIJONO" -- "tämäonmerkkijono"
```

## Katso myös

- [Elm String -dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Unicode-standardi](https://unicode.org/)
- [Elm-kurssimateriaalit suomeksi](https://stichydev.github.io/elm-kurssi-fi/)
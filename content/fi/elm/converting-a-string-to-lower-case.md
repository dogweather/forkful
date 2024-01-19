---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Muunnetaan merkkijono pieniksi kirjaimiksi tarkoittaa, että kaikki merkkijonon isot kirjaimet korvataan pienillä kirjaimilla. Tämä on hyödyllistä, kun haluat esimerkiksi vertailla merkkijonoja riippumatta kirjoituskoosta.

## Näin se tehdään:
Elm tarjoaa valmiin funktion muuttamaan merkkijonon pieniksi kirjaimiksi. Kas näin:

```
import String

lowercaseString : String -> String
lowercaseString string = 
    String.toLower string

main =
    print (lowercaseString "HELLO, WORLD!")
```

Tämän ohjelman tulostus olisi:

```
"hello, world!"
```

## Syvempi sukellus:
Alkuperäisessä ASCII-standardissa erot isoilla ja pienillä kirjaimilla eivät olleet merkittäviä ja ne vertailtiin usein sivuuttamalla koko. Tämä vanha perinne jatkuu edelleen ohjelmointikielissä.
Vaihtoehtona yllä mainitulle funktiolle on käyttää seuraavaa koodinpätkää, jos haluat hallita prosessia tarkemmin.

```
import Char

lowercaseCustom : String -> String
lowercaseCustom string = 
    String.fromList <| List.map Char.toLower <| String.toList string
```

Tämä funktio käy läpi jokaisen kirjaimen merkkijonossa yksitellen ja muuttaa sen pieneksi kirjaimeksi. Sillä voi olla parempi suorituskyky, jos merkkijonot ovat erittäin pitkiä.

## Katso myös:
* Elm String moduuli dokumentaatio: https://package.elm-lang.org/packages/elm/core/latest/String
* Elm Char moduuli dokumentaatio: https://package.elm-lang.org/packages/elm/core/latest/Char
* Sivu merkkijonojen vertailusta: https://en.wikipedia.org/wiki/String_(computer_science)#:~:text=In%20computer%20programming%2C%20a%20string,such%20as%20semantic%20version%20numbers.
---
title:                "Yhdistävien merkkijonojen yhdistäminen"
html_title:           "Haskell: Yhdistävien merkkijonojen yhdistäminen"
simple_title:         "Yhdistävien merkkijonojen yhdistäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit yhdistää merkkijonoja Haskellilla? On monia tilanteita, joissa on tarpeen yhdistää erillisiä merkkijonoja yhdeksi kokonaisuudeksi. Esimerkiksi voit käyttää sitä luomaan dynaamisia lausekkeita tai tulostamaan muotoiltuja tekstejä.

## Miten

Yksinkertaisimmassa muodossaan merkkijonojen yhdistäminen tapahtuu käyttämällä `++` operaattoria. Tämä yhdistää kaksi merkkijonoa yhdeksi uudeksi merkkijonoksi. Voit myös yhdistää useita merkkijonoja kerralla käyttämällä `concat` funktion avulla.

```Haskell
"Hello " ++ "World!" -- "Hello World!"

concat ["Hello ", "World!"] -- "Hello World!"
```

Yhdistämisen lisäksi voit myös tehdä muotoiltuja merkkijonoja käyttämällä `printf` funktion avulla. Tämä toimii samalla tavalla kuin C-kielen `printf` funktio.

```Haskell
printf "%s %s!" "Hello" "World" -- "Hello World!"
```

## Syvempi sukellus

Haskellissa merkkijonot ovat listoja, joiden alkioina ovat merkit. Siksi merkkijonojen yhdistäminen käyttäen `++` operaattoria on samanlaista kuin listojen liittäminen. Tämä mahdollistaa myös muiden listafunktioiden käytön merkkijonojen kanssa, kuten `map` ja `filter`.

```Haskell
map toUpper "hello" -- "HELLO"

filter isDigit "abc123" -- "123"
```

Haskellissa on myös `Text` tyyppi, joka tarjoaa tehokkaamman tavan käsitellä isoja merkkijonoja. Voit muuntaa merkkijonon `Text` tyyppiin ja takaisin käyttämällä `pack` ja `unpack` funktioita.

```Haskell
import Data.Text

let text = pack "Hello"
unpack text -- "Hello"
```

## Katso myös

- [String-tyyppi Haskellin dokumentaatiossa](https://www.haskell.org/tutorial/strings.html)
- [Data.Text-moduulin dokumentaatio](https://hackage.haskell.org/package/text/docs/Data-Text.html)
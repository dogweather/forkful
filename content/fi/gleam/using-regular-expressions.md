---
title:    "Gleam: Säännöllisten lausekkeiden käyttö"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

On olemassa monia skenaarioita, joissa säännöllisiä lausekkeita (regular expressions) voidaan käyttää Gleam-ohjelmoinnissa. Esimerkiksi, jos haluat suodattaa tai korvata tiettyjä merkkijonoja, käsitellä tietokannan sisältöä tai validoida käyttäjän antamia syötteitä.

## Miten

Gleamilla voidaan käyttää säännöllisiä lausekkeita käyttämällä Gleam Regex -moduulia. Esimerkiksi, jos haluat etsiä ja korvata kaikki vokaalit merkkijonossa, voit tehdä sen seuraavasti:

```Gleam
import gleam/regex

let pattern = regex.compile(~pattern="[aeiou]")
let result = regex.replace(~pattern, ~replacement="", ~subject="Hello World")
```

Tuloksena saadaan "Hll Wrld". Voit myös käyttää säännöllisiä lausekkeita suoraan merkkijonossa käyttämällä =~ operaattoria:

```Gleam
import gleam/regex

let result = "Hello World" =~ "[aeiou]"

```

Tuloksena saadaan "true", jos kyseisen säännöllisen lausekkeen mukaan löytyy vastaava merkkijono, muuten "false".

## Syvemmälle

Säännölliset lausekkeet ovat voimakas työkalu, joka kulkee käsi kädessä Gleamin kanssa, koska molemmat muuttujat ovat staattisesti tyypitettyjä. Tämä tarkoittaa, että voit ennalta määrittää tiedon tyypin ja välttää mahdolliset bugeja koodissasi. Gleam Regex -moduulilla on myös monia muita hyödyllisiä funktioita, joita voit käyttää, kuten regex.split ja regex.match.

## Katso myös

- Gleam Regex moduulin dokumentaatio: [https://gleam.run/modules/regex/](https://gleam.run/modules/regex/)
- Säännöllisten lausekkeiden opetusohjelma Gleamille: [https://medium.com/swlh/regular-expressions-in-gleam-8521ba718e94](https://medium.com/swlh/regular-expressions-in-gleam-8521ba718e94)
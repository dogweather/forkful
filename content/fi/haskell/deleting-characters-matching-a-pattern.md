---
title:                "Haskell: Asetusten vastaavien merkkien poistaminen"
simple_title:         "Asetusten vastaavien merkkien poistaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin tehtävissä voi olla tarpeen poistaa merkkejä tietystä kuvioista teksteistä. Tämä voi olla hyödyllistä esimerkiksi tietojenpuhdistuksessa tai tiedostojen muokkaamisessa. Haskell tarjoaa tehokkaan ja helppokäyttöisen menetelmän poistaa merkkejä tietystä kuvioista käyttäen `Data.List` kirjastoa.

## Kuinka tehdä se

Aluksi meidän täytyy ladata `Data.List` kirjasto käyttöömme `import`-käskyllä. Sitten voimme käyttää `deleteBy` funktiota, joka ottaa kaksi argumenttia: poistettavan kuvion ja listan, josta haluamme poistaa kyseisen kuvion. Esimerkiksi, jos haluamme poistaa kaikki numerot merkkijonosta, voimme käyttää seuraavaa koodia:

```Haskell
import Data.List

let merkkijono = "123abc456"
let numerot = ['0','1','2','3','4','5','6','7','8','9']

deleteBy (\x y -> x == y) numerot merkkijono
```

Tämä antaisi meille tulokseksi `"abc"` eli merkkijonon missä kaikki numerot on poistettu. Voimme myös asettaa erilaisia kriteereitä poistamiselle, esimerkiksi poistaa vain merkit `a-f` annetusta merkkijonosta käyttäen `isLower` ja `isAlpha` funktioita.

## Syvemmälle aiheeseen

Haskellissa on myös muita hyödyllisiä funktioita, kuten `delete` ja `deleteFirstsBy`, jotka voivat poistaa tietyn merkin tai useita merkkejä listalta. `delete` poistaa kaikki esiintymät annetusta merkistä, kun taas `deleteFirstsBy` poistaa vain ensimmäisen esiintymän. Näitä funktioita voivat olla hyödyllisiä esimerkiksi silloin kun haluamme poistaa tietyn merkin useista merkkijonoista tai tarkemmin määritellä mitkä merkit poistetaan.

## Katso myös

- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)
- [Data.List kirjaston dokumentaatio](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)
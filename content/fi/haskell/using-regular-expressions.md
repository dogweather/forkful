---
title:                "Haskell: Säännöllisten lausekkeiden käyttäminen"
simple_title:         "Säännöllisten lausekkeiden käyttäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Miksi käyttää regulaaarisia lausekkeita?

Jos olet koskaan joutunut etsimään tiettyä merkkijonoa tekstistä tai halunnut korvata kaikki esiintymät jollakin toisella merkkijonolla, olet todennäköisesti törmännyt käsitteeseen "regulaariset lausekkeet". Nämä ovat erittäin hyödyllisiä työkaluja, joita voit käyttää helpottamaan tiettyjen merkkijonojen käsittelyä ohjelmoinnissa.

## Miten käyttää regulaaarisia lausekkeita Haskellissa?

Haskellissa regulaaarisia lausekkeita käsitellään `Text.Regex.Posix` -kirjaston avulla. Voit aloittaa käyttämällä `=~` -funktiota, joka tarkistaa, vastaako merkkijono sille annettua säännöllistä lauseketta.

`example.hs`

```Haskell
import Text.Regex.Posix

main = do
  let str = "Tämä on esimerkki"
  let regex = "esimerkki"
  if str =~ regex :: Bool
    then putStrLn "Löytyi!"
    else putStrLn "Ei löytynyt."
```

```Haskell
Löytyi!
```

Voit myös käyttää regulaarisia lausekkeita korvaamaan tiettyjä merkkijonoja `subRegex` -funktiolla.

```Haskell
import Text.Regex.Posix

main = do
  let str = "Tämä on esimerkki"
  let regex = "esimerkki"
  let replacement = "esimerkkilause"
  let newStr = subRegex (makeRegex regex) str replacement
  putStrLn newStr
```

```Haskell
Tämä on esimerkkilause
```

## Syvempi sukellus regulaaaristen lausekkeiden käyttöön

Regulaaaristen lausekkeiden kanssa on monia erilaisia symboleja ja merkityksiä, joten niiden oppiminen voi tuntua aluksi hämmentävältä. On hyödyllistä tutkia esimerkkejä ja kokeilla erilaisia säännöllisiä lausekkeita, jotta ymmärtäisit paremmin niiden toimintaa.

Voit myös hyödyntää `=~` -funktion tarjoamaa syntaksin muotoilua, jossa voit käyttää `::` -operaattoria asettaaksesi muuttujalle tietyn tyyppisen arvon. Tämä auttaa varmistamaan, että säännöllinen lauseke vastaa oikeaa tyyppiä.

```Haskell
let str = "12345"
if str =~ "^[0-9]+$" :: Bool
  then putStrLn "Sisältää vain numeroita!"
  else putStrLn "Sisältää muita merkkejä."
```

```Haskell
Sisältää vain numeroita!
```

## Katso myös

- [Haskellin virallinen dokumentaatio regulaaarisista lausekkeista](https://www.haskell.org/onlinereport/stdprelude.html#l:Rld-260420103302)
- [Regulaaristen lausekkeiden opas (englanniksi)](https://www.regular-expressions.info/)
---
title:                "Työskentely jsonin kanssa"
html_title:           "Haskell: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Haskell on nykypäivänä yksi suosituimmista ohjelmointikielistä, ja sen käyttö JSON-tiedostojen käsittelyyn on erittäin tehokasta ja helppoa. JSON on myös laajasti käytetty tiedostomuoto, joten sen kanssa työskentely voi olla tarpeellista monille ohjelmoijille.

## Miten

Haskellissa JSON-tiedoston lukeminen ja kirjoittaminen tapahtuu käyttämällä Data.Aeson -kirjastoa. Alla on esimerkki koodista, joka lukee JSON-tiedoston ja tulostaa sen sisällön:

```Haskell
import Data.Aeson
import Data.ByteString.Lazy

main = do 
    fileContents <- ByteString.Lazy.readFile "example.json"
    let decodedJSON = decode fileContents :: Maybe Value
    case decodedJSON of 
        Just contents -> print contents
        Nothing -> print "Error decoding JSON"
```

Kuten nähdään, meidän täytyy ensin lukea tiedosto bytestring-muodossa ja sitten käyttää `decode` -funktiota muuttamaan se `Value`-muodoksi. Tässä tapauksessa käytämme `Maybe`-tyyppiä, jotta voimme käsitellä virhetilanteet.

Seuraavaksi esimerkissä näytämme, kuinka voimme luoda ja kirjoittaa JSON-tiedoston sisällön:

```Haskell
import Data.Aeson
import Data.ByteString.Lazy

main = do 
    let json = object ["name" .= "John", "age" .= (30 :: Int), "hobbies" .= ["hiking", "coding"]]
    let byteString = encode json
    ByteString.Lazy.writeFile "new.json" byteString
```

Täällä me ensin luomme `object`-funktiolla JSON-arvon ja sitten käytämme `encode`-funktiota muuttamaan sen bytestring-muotoon. Lopuksi kirjoitamme sisällön uuteen tiedostoon.

## Deep Dive

Haskellissa JSON-tietueet ovat dynaamisia ja samanlaisia kuin JavaScriptin objektit. Ne ovat kuitenkin staattisesti tyypitettyjä ja voimme käyttää vahvistinta (`::`) määrittelemään, millainen tietotyyppi haluamme. Lisäksi voimme käyttää `.`-merkkiä hakeaksemme tietueesta tietyn kentän. Esimerkiksi jos haluamme hakea nimen JSON-tietueesta, käytämme `json . name`.

See Also

- Data.Aeson dokumentaatio: https://hackage.haskell.org/package/aeson-1.4.5.0/docs/Data-Aeson.html
- Haskell Wikibooks: https://en.wikibooks.org/wiki/Haskell
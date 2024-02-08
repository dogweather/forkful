---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
aliases:
- fi/elm/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:07.352035-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä & Miksi?"
Muuttaa merkkijonon pieniksi kirjaimiksi. Käytetään yhdenmukaistamaan dataa, esimerkiksi vertailuun tai syötteen käsittelyyn.

## How to:
"Kuinka:"
```Elm
import String

lowercaseString = String.toLower "HeI MAaILmA"
-- Tulos: "hei maailma"
```

## Deep Dive
"Sukellus syvyyksiin"
Historiallisesti merkkijonon pienentäminen on peräisin ajoilta, jolloin ohjelmistojen piti vertailla tekstiä ilman kirjainkokoon liittyvää herkkyyttä. Elm käyttää UTF-16 -koodauksen piirteitä muuttaakseen kirjaimet pieniksi. Vaihtoehtoina voisi käyttää esimerkiksi regex-muunnoksia, mutta `String.toLower` on useimmiten tehokkain ja suositeltavin tapa Elm:ssä.

Stringin pienentämisessä Elm tekee yhteistyötä selaimen kanssa, käyttäen sen natiivia string-funktionaliteettia, mikä takaa suorituskyvyn ja yksinkertaisuuden.

## See Also
"Katso myös"
- Elm String -kirjaston dokumentaatio: [Elm String toLower](http://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Unicode standardin selitys kirjainkoosta: [The Unicode Standard Case Folding](http://unicode.org/reports/tr21/tr21-5.html)

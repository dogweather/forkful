---
date: 2024-01-20 17:38:07.352035-07:00
description: "How to: \"Sukellus syvyyksiin\" Historiallisesti merkkijonon pienent\xE4\
  minen on per\xE4isin ajoilta, jolloin ohjelmistojen piti vertailla teksti\xE4 ilman\u2026"
lastmod: '2024-04-05T21:53:58.040799-06:00'
model: gpt-4-1106-preview
summary: "\"Sukellus syvyyksiin\" Historiallisesti merkkijonon pienent\xE4minen on\
  \ per\xE4isin ajoilta, jolloin ohjelmistojen piti vertailla teksti\xE4 ilman kirjainkokoon\
  \ liittyv\xE4\xE4 herkkyytt\xE4."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

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

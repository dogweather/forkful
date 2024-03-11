---
date: 2024-01-20 17:38:07.352035-07:00
description: "\"Mit\xE4 & Miksi?\" Muuttaa merkkijonon pieniksi kirjaimiksi. K\xE4\
  ytet\xE4\xE4n yhdenmukaistamaan dataa, esimerkiksi vertailuun tai sy\xF6tteen k\xE4\
  sittelyyn."
lastmod: '2024-03-11T00:14:30.417286-06:00'
model: gpt-4-1106-preview
summary: "\"Mit\xE4 & Miksi?\" Muuttaa merkkijonon pieniksi kirjaimiksi. K\xE4ytet\xE4\
  \xE4n yhdenmukaistamaan dataa, esimerkiksi vertailuun tai sy\xF6tteen k\xE4sittelyyn."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
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

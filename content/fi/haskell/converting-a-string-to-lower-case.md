---
title:                "Haskell: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? On monia tapauksia, joissa tämä voi olla hyödyllistä, kuten tietokoneohjelmointi tai tekstikäsittely.

## Miten

Haskell tarjoaa meille kätevän toiminnon, `toLower`, joka muuttaa merkkijonon pieniksi kirjaimiksi. Voit käyttää sitä seuraavasti:

```Haskell
toLower :: String -> String
toLower "LOMA"  -- palauttaa "loma"
toLower "Mökki" -- palauttaa "mökki"
```

Mutta miksi toLower on tärkeämpi kuin yksinkertaisesti miettiä merkkijonon kirjainten pienentämistä? Haskellin funktionaalisen ohjelmoinnin yksi periaatteista on välttää sivuvaikutuksia, mikä tarkoittaa, että funktiot eivät muuta alkuperäisiä syötteitä vaan palauttavat uusia arvoja.

## Syväsukellus

ToLower toimii hyvin kaikkien merkkijonon kirjainten muuttamiseen pieniksi kirjaimiksi, mutta miten se toimii? On olemassa muutamia tapoja, joilla se voitaisiin toteuttaa, mutta yksi yleisimmistä keinoista on käyttää ASCII-taulukkoa kirjainten muuttamiseen.

ASCII-taulukko on taulukko, jossa jokaiselle merkkijonon symbolille on annettu numerollinen arvo. Pienimmät arvot ovat varattu erikoismerkeille ja isommat kirjaimille. Pienet kirjaimet ovat aina suurempia kuin isoilla kirjaimilla.

ToLower-funktio käy läpi merkkijonon jokaisen kirjaimen ja muuttaa arvon pienemmäksi, jos se on iso kirjain. Lopuksi palautetaan uusi merkkijono muuttuneilla arvoilla.

## Katso myös

- [Haskell dokumentation - toLower](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html#g:3)
- [ASCII-taulukko](https://fi.wikipedia.org/wiki/ASCII)
- [Funktionaalinen ohjelmointi](https://fi.wikipedia.org/wiki/Funktionaalinen_ohjelmointi)
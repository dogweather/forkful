---
title:                "Haskell: Vianmääritystulostus"
simple_title:         "Vianmääritystulostus"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Miksi: Miksi tulostamme debug-ilmoituksia?

Joskus koodia kirjoittaessa tai korjatessa törmäämme ongelmiin, joita on vaikea havaita vain koodia lukemalla. Tällöin debug-ilmoitusten tulostaminen voi auttaa meitä selvittämään mistä ongelma johtuu ja missä kohdassa koodia se tapahtuu.

## Miten: Näin tulostat debug-ilmoituksia Haskellissa

Haskellissa tulostamme debug-ilmoituksia käyttäen `putStrLn` -funktiota. Tämä funktio ottaa parametrinaan merkkijonon, jonka se sitten tulostaa näytölle. Alla on yksinkertainen esimerkki siitä, miten käytämme `putStrLn` -funktiota:

```Haskell
putStrLn "Tämä on debug-ilmoitus"
```

Tämän koodin suorittamisen jälkeen näytölle tulostetaan "Tämä on debug-ilmoitus".

Debug-ilmoituksia tulostettaessa on hyvä antaa jotain hyödyllistä informaatiota, kuten muuttujien arvoja tai mitä koodin osaa suoritetaan. Tämä auttaa meitä hahmottamaan koodin toimintaa ja löytämään mahdollisia ongelmakohtia.

## Syvemmälle: Tietoa debug-ilmoitusten tulostamisesta

On tärkeää muistaa, että debug-ilmoitukset ovat hyödyllisiä vain kehitysvaiheessa ja niitä ei tulisi jättää koodiin valmiiden ohjelmien mukana. Liiallinen debug-ilmoitusten tulostaminen voi hidastaa ohjelman suoritusta ja vaikeuttaa koodin lukemista.

Parhaiten debug-ilmoitusten tulostamista varten kannattaa luoda oma funktio, joka hyödyntää `putStrLn` -funktiota ja ottaa parametrinaan ne arvot, joita halutaan tulostaa. Näin koodi pysyy siistinä ja selkeänä.

## Katso myös

- [Haskellin debuggaaminen](https://haskell.org/debugging)
- [putStrLn dokumentaatio](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:putStrLn)
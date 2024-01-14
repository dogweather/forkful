---
title:    "Elm: Kuvion mukaisten merkkien poistaminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko oppia kuinka poistaa merkkejä tietystä kuvioista Elm-ohjelmoinnissa? Tässä artikkelissa kerromme syyt miksi haluat ehkä tehdä niin ja kuinka se voi auttaa sinua kuvaamaan tiettyjä data-tyyppejä paremmin.

## Kuinka tehdä se

Poistaminen merkkejä tietyistä kuvioista on helppoa ja kätevää Elm-kielellä. Katso alla oleva koodiesimerkki lähemmin ymmärtääksesi kuinka se toteutetaan:

```Elm
-- Luo esimerkkidata
matriisi = [ "ABC", "DEF", "GHI" ]

-- Määritä kuvio jota haluat poistaa, tässä tapauksessa "C"
kuvio = "C"

-- Käytä "filter" funktiota ja lambda-lauseketta poistaa kuvio
uusi_matriisi = List.filter (\x -> not (String.contains kuvio x)) matriisi

-- Tulostetaan uusi matriisi konsoliin
Debug.toString uusi_matriisi
```
Yllä olevassa esimerkissä luomme listan "matriisi" joka sisältää kolme sanaa. Määrittelemme sitten poistettavan kuvion ja käytämme "filter" funktiota ja lambda-lauseketta poistaaksemme sen jokaisesta sanasta. Lopuksi tulostamme uuden matriisin konsoliin nähdäksemme tuloksen. Alla oleva tulos näyttää miten kuvio "C" on poistettu jokaisesta sanasta:

```
["AB", "DEF", "GHI"]
```

## Syvempää tietoa

Voit myös käyttää samaa tekniikkaa poistaaksesi merkkejä muista kuvioista, kuten esimerkiksi kokonaisia sanoja tai lauseita. Voit myös yhdistää "filter" funktion muihin Elm-ohjelmointikielen ominaisuuksiin, kuten "map" ja "fold", saadaksesi monimutkaisempia ja tehokkaampia ratkaisuja. Kokeile erilaisia vaihtoehtoja ja löydä sinulle sopivin tapa poistaa merkkejä tietystä kuvioista!

## Katso myös

- [String-moduuli Elmissä](https://package.elm-lang.org/packages/elm-lang/core/latest/String)
- [Stringin käsittely Elm-ohjelmoinnissa](https://elmprogramming.com/string-manipulation.html)
- [Lambda-lausekkeet Elm-ohjelmoinnissa](https://elmprogramming.com/anonymous-functions.html)
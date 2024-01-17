---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Elm: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

Hakeminen ja tekstin korvaaminen tarkoittavat tekstin etsimistä ja sen vaihtamista jollain toisella tekstillä. Ohjelmoijat tekevät tätä esimerkiksi korjatessaan virheitä koodissaan tai muokatessaan tekstiä käyttöliittymissä.

Miten:

Elm tarjoaa meille muutamia tapoja suorittaa hakemista ja tekstin korvaamista. Voimme käyttää sisäistä funktiota ```String.replace```, kuten alla:

```Elm
String.replace "eka" "kaksi" "eka teksti"
```

Tämä palauttaa tekstin ```kaksi teksti```. Voimme myös käyttää säännöllisiä lausekkeita ```Regex.replace```, esimerkiksi:

```Elm
Regex.replace (Regex.regex "a([\\w]*)i") (\match -> "x" ++ match) "Hei kaikki, mitä kuuluu?"
```

Tämä palauttaa tekstin ```Hei kxlli, mitä kuuluu?```.

Deep Dive:

Tekstin hakemisen ja korvaamisen historiassa on useita eri vaihtoehtoja ja algoritmeja. Yleisesti ottaen nämä toiminnot ovat olleet hyödyllisiä ohjelmistokehittäjille ja tekstilisäosien suunnittelijoille. Nykyään monilla ohjelmointikielillä, kuten Elmillä, on sisäänrakennettuja työkaluja näiden toimintojen suorittamiseksi.

See Also:

Lisätietoja hakemisesta ja korvaamisesta sekä siihen liittyviä resursseja löydät näistä linkeistä:

- Elm String -moduuli: https://package.elm-lang.org/packages/elm/core/latest/String
- Regex -moduuli: https://package.elm-lang.org/packages/elm/regex/latest/Regex
- Tekstin hakeminen ja korvaaminen: https://www.w3schools.com/jsref/jsref_replace.asp
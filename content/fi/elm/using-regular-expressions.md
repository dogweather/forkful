---
title:    "Elm: Säännöllisten lausekkeiden käyttö"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita ohjelmoinnissa?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja ohjelmoinnissa, sillä niiden avulla voidaan tarkistaa ja manipuloida tekstejä. Ne ovat erityisen hyödyllisiä sellaisissa tehtävissä, kuten tietojen validoinnissa ja hakemisessa suurista tekstimassoista.

## Näin käytät säännöllisiä lausekkeita Elm-ohjelmoinnissa

Säännöllisiä lausekkeita käytetään Elm-kielen Regex-paketin avulla. Ennen kuin voit käyttää Regex-pakettia, sinun tulee asentaa se käyttämällä `elm install elm-community/regex` komentoa.

Seuraavassa esimerkissä käytetään säännöllistä lauseketta tarkistamaan, onko annettu sähköpostiosoite kelvollinen. Säännöllinen lauseke tarkistaa, että osoite sisältää @-merkin ja vähintään yhden pisteen, jotta se olisi kelvollinen.

```elm
import Regex exposing (find, regex, match)

emailRegex = regex "[A-Za-z0-9._%+-]@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

validEmail = match emailRegex "example@email.com"

invalidEmail = match emailRegex "example@"

-- validEmail = Just True
-- invalidEmail = Nothing
```

Yllä olevassa koodissa ensin tuodaan Regex-paketin tarvittavat toiminnot. Sitten luodaan säännöllinen lauseke regex-funktiolla ja tarkistetaan sähköpostiosoitteet match-funktiolla. Toisessa tapauksessa tuloksena on `Just True`, mutta kolmannessa tapauksessa `Nothing`, sillä sähköpostiosoite on kelvoton.

## Syvempi sukellus säännöllisiin lausekkeisiin

Säännölliset lausekkeet koostuvat erilaisista merkeistä ja symboleista, joilla voidaan määrittää tietyt tekstikäytänteet. Esimerkiksi hakasulkeet `[]` voi käyttää määrittämään sallitut merkit, kuten `[a-z]` tarkoittaa mitä tahansa pientä kirjainta aasta z:iin. Asteriski `*` taas tarkoittaa, että edellinen merkki voi esiintyä nolla tai useamman kerran.

On myös hyödyllistä huomata, että Regex-paketin `match`-funktio palauttaa joko `Just` tai `Nothing` arvon. `Just` arvo tarkoittaa, että säännöllinen lauseke vastaa annettua tekstiä ja `Nothing` arvo tarkoittaa, että säännöllinen lauseke ei vastaa tekstiä.

## Katso myös

- [Regex-paketin dokumentaatio](https://package.elm-lang.org/packages/elm-community/regex/latest/)
- [Säännölliset lausekkeet Wikipediassa](https://fi.wikipedia.org/wiki/S%C3%A4%C3%A4nn%C3%B6llinen_lauseke)
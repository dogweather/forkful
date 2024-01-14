---
title:                "Elm: Etsiminen ja tekstien korvaaminen"
simple_title:         "Etsiminen ja tekstien korvaaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sitten haluaisit vaihtaa ja etsiä tekstiä vaihdettavaksi? Yksi syy voisi olla tehokkuus - käyttämällä oikeita työkaluja säästät aikaa ja vaivaa käsitellessäsi suuria määriä tekstiä. Olipa kyseessä sitten pienen kooditiedoston muokkaaminen tai suuren dokumentin läpikäyminen, hakukomennot voivat olla todella hyödyllisiä.

## Kuinka tehdä näin

Pidä selvää sinapista ja kovasta usein käyttämistäsi hakukomentoistasi luomalla funktio, joka yhdistää tarvittavat toiminnot yhteen. Esimerkiksi, jos haluat korvata kaikki "he" sanat "she" sanoilla, voit käyttää seuraavaa funktiota:

```Elm
replace : String -> String
replace string =
    String.replace "he" "she" string
```

Tämä funktio ottaa merkkijonon `string` argumenttina ja korvaa kaikki "he" sanat "she" sanoilla. Voit myös käyttää muita tekstinhakukomentoja, kuten `String.contains`, `String.startsWith` ja `String.endsWith`.

## Syvemmälle

On tärkeää muistaa, että hakukomennot voivat olla joustavia ja niitä voi käyttää monimutkaisempien tehtävien suorittamiseen. Voit esimerkiksi käyttää `String.split` ja `String.join` komentoja yhdistettynä hakukomentoihin erotellaksesi ja yhdistääksesi tekstiä haluamallasi tavalla.

Lisäksi voit käyttää säännöllisiä lausekkeita (`Regex`) tekstin hakemiseen ja korvaamiseen vielä monipuolisemmin. Säännöllisten lausekkeiden käyttö ei ole välttämätöntä, mutta se voi olla hyödyllistä monimutkaisemmissa tapauksissa.

## Katso myös

- [Elm-documentation](https://elm-lang.org/docs) - lisätietoja tekstinkäsittelystä ja käytettävissä olevista komentoriveistä.
- [Elm-repl](https://repl.it/languages/elm) - voit kokeilla hakukomentoja interaktiivisessa ympäristössä.
- [Elm live-ohje](https://guide.elm-lang.org/) - oppitunteja ja käytännön esimerkkejä hakukomennoista ja muista textinkäsittelytekniikoista.
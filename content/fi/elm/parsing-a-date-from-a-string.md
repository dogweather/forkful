---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:35:58.978280-07:00
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 
"Mikä ja Miksi?"

Jäsennämme päivämäärän merkkijonosta, koska tieto on usein vuorovaikutuksessa teksti-muodossa, ja meidän on käsiteltävä sitä ohjelmallisesti. Esimerkiksi kun käyttäjä syöttää päivämäärän tai lataamme dataa palvelimelta.

## How to:
"Näin se toimii:"

Elm tarjoaa puhtaan syntaksin ja type turvallisuuden, mutta se ei sisällä sisäänrakennettua päivämäärän jäsennystä. Käytämme usein `elm/time` kirjastoa yhdessä `justinmimbs/date` kanssa.

```Elm
import Time
import Date exposing (Date)
import Date.Extra.Parse exposing (iso8601)

parseDate : String -> Result String Date
parseDate dateStr =
    dateStr |> iso8601

-- Esimerkiksi käytössä:
case parseDate "2021-04-23T18:25:43.511Z" of
    Ok date -> 
        -- jatka päivämäärän kanssa
    Err errorMessage ->
        -- käsittele virhettä
```

## Deep Dive
"Syväsukellus:"

Päivämäärän jäsennys Elm:ssä ei ole niin suoraviivaista kuin joissain muissa kielissä. Alkujaan Elm ei tarjonnut vahvoja päivämääräkäsittelyn työkaluja, joten yhteisö luo kirjastoja, kuten `justinmimbs/date`.

Vaihtoehtoja on muitakin, esimerkiksi `ryannhg/date-format`, joka tarjoaa funktioita päivämäärän muotoiluun. Nämä kirjastot nojaavat `elm/time`-pakettiin, mutta laajentavat sen toiminnallisuutta.

Päivämäärien käsittelyssä on myös aikavyöhykkeiden ja lokaalin mukaisten esitystapojen huomioiminen. `justinmimbs/date`-kirjastossa `iso8601`-funktio ymmärtää ISO 8601 -muotoisia merkkijonoja ja palauttaa `Result`-tyypin, mikä auttaa virheenkäsittelyssä.

## See Also
"Katso myös:"

- Elm Time library documentation: [packages.elm-lang.org/packages/elm/time/latest](https://package.elm-lang.org/packages/elm/time/latest)
- Justin Mimbs's Date library on GitHub: [github.com/justinmimbs/date](https://github.com/justinmimbs/date)
- Ryan's date-format for string formatting: [github.com/ryannhg/date-format](https://github.com/ryannhg/date-format)

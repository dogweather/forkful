---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:24.335405-07:00
description: "Kuinka: Elm:ll\xE4 ei ole sis\xE4\xE4nrakennettua yht\xE4 vahvaa kyky\xE4\
  \ p\xE4iv\xE4m\xE4\xE4rien j\xE4sent\xE4miseen kuin joillakin muilla kielill\xE4\
  , vaan se p\xE4\xE4asiassa tukeutuu\u2026"
lastmod: '2024-03-13T22:44:56.498079-06:00'
model: gpt-4-0125-preview
summary: "Elm:ll\xE4 ei ole sis\xE4\xE4nrakennettua yht\xE4 vahvaa kyky\xE4 p\xE4\
  iv\xE4m\xE4\xE4rien j\xE4sent\xE4miseen kuin joillakin muilla kielill\xE4, vaan\
  \ se p\xE4\xE4asiassa tukeutuu JavaScript-yhteentoimivuuteen tai kirjastoihin monimutkaisempia\
  \ operaatioita varten."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
weight: 30
---

## Kuinka:
Elm:llä ei ole sisäänrakennettua yhtä vahvaa kykyä päivämäärien jäsentämiseen kuin joillakin muilla kielillä, vaan se pääasiassa tukeutuu JavaScript-yhteentoimivuuteen tai kirjastoihin monimutkaisempia operaatioita varten. Kuitenkin voit käyttää `elm/time` pakettia perusjäsentämiseen, ja monimutkaisempia tarpeita varten kolmannen osapuolen `justinmimbs/date` kirjastoa suositellaan laajalti.

### Jäsentäminen käyttäen `elm/time`:
`elm/time` tarjoaa `Time` moduulin, jonka avulla voit työskennellä aikaleimojen kanssa ihmisen luettavien päivämäärien sijaan. Vaikka se ei suoraan jäsentää päivämääriä merkkijonoista, voit muuntaa ISO 8601 merkkijonon POSIX aikaleimaksi, jonka kanssa sitten voit työskennellä.

```elm
import Time exposing (Posix)

-- Oletetaan, että sinulla on ISO 8601 päivämäärämerkkijono
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- Muunna se POSIX aikaleimaksi (tämä funktio palauttaa `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- Esimerkkituloste: Ok <posix aika-arvo>
```

### Jäsentäminen käyttäen `justinmimbs/date`:
Monimutkaisempiin jäsentämisiin, kuten ei-ISO formaattien käsittelyyn, `justinmimbs/date` kirjasto on erinomainen valinta. Tässä on miten voit käyttää sitä mukautetun päivämäärämerkkijonon jäsentämiseen:

1. Varmista, että sinulla on kirjasto asennettuna:

```shell
elm install justinmimbs/date
```

2. Käytä `Date.fromString` funktiota mukautettujen päivämääräformaattien jäsentämiseen:

```elm
import Date
import Result exposing (Result(..))

-- Sanotaan, että sinulla on mukautettu päivämäärämerkkijono formaatti `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- Funktio mukautetun formaatin jäsentämiseen
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- Esimerkkikäyttö
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- Esimerkkituloste: Ok (Date.fromCalendarDate 2023 Jan 1)
```

Näissä esimerkeissä `Result` tyyppi kapseloi joko onnistuneen jäsentämisen, joka tuottaa päivämäärän (`Ok`) tai virheen (`Err`), mahdollistaen vahvan virheenkäsittelyn Elm-sovelluksissasi.

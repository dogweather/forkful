---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:59.527923-07:00
description: "Kuinka: Elmiss\xE4 ei ole sis\xE4\xE4nrakennettua funktiota merkkijonojen\
  \ alkukirjaimen suurentamiseen. Voit kuitenkin saavuttaa t\xE4m\xE4n helposti k\xE4\
  ytt\xE4m\xE4ll\xE4\u2026"
lastmod: '2024-03-13T22:44:56.471245-06:00'
model: gpt-4-0125-preview
summary: "Elmiss\xE4 ei ole sis\xE4\xE4nrakennettua funktiota merkkijonojen alkukirjaimen\
  \ suurentamiseen."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Kuinka:
Elmissä ei ole sisäänrakennettua funktiota merkkijonojen alkukirjaimen suurentamiseen. Voit kuitenkin saavuttaa tämän helposti käyttämällä sisäänrakennettuja `String` moduulin funktioita kuten `toUpper`, `toLower`, `left`, ja `dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Esimerkin käyttö
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Tuloste: "Hello World"
```

Monimutkaisemmissa tilanteissa tai jos haluat käyttää kirjastoa, joka tarjoaa suoran tavan suurentaa merkkijonojen alkukirjaimia, harkitse kolmannen osapuolen paketin, kuten `elm-community/string-extra`, käyttöä. Kuitenkin viimeisimmän päivitykseni mukaan, Elmin ekosysteemi kannustaa käsittelemään tällaisia tehtäviä käyttämällä sisäänrakennettuja funktioita pitääkseen kielen ja projektit yksinkertaisina.

```elm
import String.Extra as StringExtra

-- Jos kolmannen osapuolen kirjastossa on `capitalize` funktio
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Esimerkin käyttö oletetun kirjastofunktion kanssa
main =
    "this is elm" |> capitalizeWithLibrary
    -- Oletettu tuloste: "This is elm"
```

Tarkista aina Elmin pakettirepositorio viimeisimmistä ja suosituimmista kirjastoista merkkijonojen käsittelyyn, jos etsit lisätoiminnallisuutta vakio kirjaston ulkopuolelta.

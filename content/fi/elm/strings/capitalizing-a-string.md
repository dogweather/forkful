---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:59.527923-07:00
description: "Merkkijonon alkukirjaimen muuttaminen isoksi muuttaa annetun merkkijonon\
  \ ensimm\xE4isen merkin isoksi kirjaimeksi samalla, kun loput merkit j\xE4tet\xE4\
  \xE4n pieniksi\u2026"
lastmod: 2024-02-19 22:05:15.365999
model: gpt-4-0125-preview
summary: "Merkkijonon alkukirjaimen muuttaminen isoksi muuttaa annetun merkkijonon\
  \ ensimm\xE4isen merkin isoksi kirjaimeksi samalla, kun loput merkit j\xE4tet\xE4\
  \xE4n pieniksi\u2026"
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon alkukirjaimen muuttaminen isoksi muuttaa annetun merkkijonon ensimmäisen merkin isoksi kirjaimeksi samalla, kun loput merkit jätetään pieniksi kirjaimiksi. Tätä tehdään usein standardoidun muotoilun tai luettavuuden vuoksi. Ohjelmoijat suorittavat tätä tehtävää usein varmistaakseen, että data esitetään johdonmukaisesti, erityisesti käyttöliittymissä tai käsiteltäessä ja näytettäessä käyttäjän syötettä.

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

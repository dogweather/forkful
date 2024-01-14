---
title:                "Elm: Tekstin etsiminen ja korvaaminen"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi etsiä ja korvata tekstejä?

Etsi ja korvaa -toiminnolla voi helposti ja nopeasti muokata useita tekstejä kerralla. Se säästää aikaa ja vaivaa, ja auttaa myös vähentämään virheiden määrää koodin kirjoittamisessa.

## Miten tehdä se Elmilla?

```Elm
-- Etsi ja korvaa "Hei" --> "Moi"
replaceHeiMoi : String -> String
replaceHeiMoi text =
    String.replace "Hei" "Moi" text
```

Tässä yksinkertaisessa esimerkissä luodaan funktio nimeltä `replaceHeiMoi`, joka ottaa parametrina merkkijonon ja palauttaa uuden merkkijonon, jossa kaikki esiintymät sanasta "Hei" korvataan sanalla "Moi".

## Syvemmälle etsimisen ja korvaamisen maailmaan

Etsi ja korvaa voidaan tehdä monilla eri tavoilla riippuen tarpeistasi. Voit esimerkiksi käyttää RegExp-kirjastoa, jonka avulla voit käyttää monimutkaisempia hakuja ja korvauksia. Voit myös käyttää `String.filter` -funktiota, joka poistaa halutut sanat kokonaan merkkijonosta.

## Katso myös

- [Elm String-dokumentaatio](https://elm-lang.org/docs/strings)
- [RegExp-kirjasto Elmissä](https://package.elm-lang.org/packages/elm/regex/latest/)
- [String.filter -funktio Elmissa](https://package.elm-lang.org/packages/elm/bytes/latest/String#filter)
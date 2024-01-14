---
title:                "Elm: Säännöllisten ilmaisujen käyttö"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat tärkeä osa ohjelmoinnin maailmaa, sillä ne tarjoavat tehokkaan tavan hakea ja muokata tekstiä. Ne ovat erityisen hyödyllisiä silloin, kun tarvitsemme tarkkoja hakuja ja muokkauksia tekstissä. Elm-ohjelmointikielessä käytävät säännölliset lausekkeet ovat erittäin tehokkaita ja niitä kannattaa opetella käyttämään.

## Kuinka käyttää säännöllisiä lausekkeita Elm-ohjelmoinnissa

Säännöllisten lausekkeiden käyttö Elm-ohjelmoinnissa tapahtuu Regex-moduulin avulla. Moduuli tarjoaa useita funktioita, jotka mahdollistavat säännöllisten lausekkeiden käytön. Seuraavan esimerkin avulla voit helposti ymmärtää, kuinka säännöllisiä lausekkeita käytetään Elm-ohjelmoinnissa:

```Elm
import Regex

-- Etsitään sana "tervetuloa" tekstistä ja korvataan se sanalla "hei"
teksti = "Tervetuloa Elm-maailmaan!"
uusiTeksti = Regex.replace (Regex.regex "tervetuloa") (always "hei") teksti
-- Tulos: "Hei Elm-maailmaan!"
```

Tässä esimerkissä tuodaan ensin Regex-moduuli ja määritellään sitten teksti, jossa haetaan ja korvataan sana. Käytämme Regex.replace-funktiota, joka ottaa ensimmäisenä parametrina etsittävän lausekkeen (Regex.regex) ja toisena parametrina muutoksen, joka halutaan tehdä (always "hei"). Lopuksi tallennamme uuden tekstin muuttujaan ja sen tuloksena saamme halutun lopputuloksen.

## Syvemmälle säännöllisten lausekkeiden käyttöön

Säännöllisten lausekkeiden syvemmän ymmärtämisen kannalta on hyödyllistä tutustua Regex-moduulin tarjoamiin eri funktioihin ja niiden käyttötapoihin. Voit esimerkiksi käyttää Regex.find-funktiota, joka hakee tietystä tekstistä ensimmäisen osuman halutulle säännölliselle lausekkeelle. Voit myös käyttää Regex.split-funktiota, joka jakaa tekstin säännöllisen lausekkeen perusteella osiin.

## Katso myös

- Elm-kielessä käytettävät säännölliset lausekkeet: https://package.elm-lang.org/packages/elm/regex/latest/
- Säännöllisten lausekkeiden opas: https://www.regular-expressions.info/
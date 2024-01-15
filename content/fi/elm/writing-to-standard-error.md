---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Elm: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheeseen ei välttämättä ole kaikkein jännittävin ohjelmointitehtävä, mutta se voi olla erittäin hyödyllistä virheiden havaitsemisessa ja korjaamisessa. Kun käytämme tätä työkalua oikein, voimme helpommin paikallistaa ja ratkaista ohjelmistoomme liittyvät ongelmat.

## Kuinka tehdä se

```Elm
import Text exposing (toPadded)
import Platform

main =
  Platform.sendToSelf "Tässä on virheilmoitus!" Nothing
  |> Task.onError (
      \error ->
        Platform.sendToSelf ("Tämä on virhe: " ++ Text.toPadded 5 ' ' error)
        Nothing
    )
```

Yllä olevassa esimerkissä näytetään, kuinka voimme käyttää Elm:n standardikirjastosta löytyvää `Platform.sendToSelf` funktiota kirjoittamaan viesti virhetilanteessa. Tämä funktio lähettää viestin suoraan selaimelle, mikä tekee siitä erittäin kätevän töitä tehdessämme.

```
> Tässä on virheilmoitus!
>     TNT   This is an error

> TNT = Tämä on virhe
```

Ylläoleva koodi luo virheilmoituksen, joka näytetään selaimen konsolissa. Tämän lisäksi virheen tiedot on myös muotoiltu ja tulostettu konsoliin, mikä tekee virheen paikantamisesta ja ratkaisemisesta helpompaa.

## Pohjustietoja

Kuten huomasimme esimerkeistä, `Platform.sendToSelf` funktiota voidaan käyttää virheilmoitusten lisäksi myös muuhun viestien lähettämiseen selaimelle. Usein tämä voi olla hyödyllistä esimerkiksi testauksessa ja debuggaamisessa. On myös hyvä muistaa, että Elm:n koodin suoritus tapahtuu pääasiassa selaimessa, joten virheviestit kulkevat suoraan siihen.

## Katso myös

- Elm:n viralliset dokumentaatiot virheenkäsittelystä: [https://elm-lang.org/docs/error](https://elm-lang.org/docs/error)
- Tietoa standardivirheestä ja sen käytöstä Elm:ssä: [https://guide.elm-lang.org/error_handling/](https://guide.elm-lang.org/error_handling/)
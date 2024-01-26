---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Standardivirheeseen kirjoittaminen tarkoittaa virheviestien lähettämistä erilliseen virhevirtsaan, joka on erillään päävirrasta. Ohjelmoijat käyttävät tätä erottelemaan normaalin tulosteen ja virheet, jotta virheitä voi käsitellä ja lokittaa tehokkaammin.

## How to:
Elmissä ei voi suoraan kirjoittaa standardivirheeseen, mutta voit logata virheitä `Debug.log` avulla. Esimerkiksi:

```Elm
import Html

main =
    let
        _ = Debug.log "Error" "Jotain meni pieleen"
    in
    Html.text "Katso konsolia virheen tiimoilta!"
```

Luo yllä oleva funktio ja avaa ohjelmasi konsoli nähdäksesi tulosteen.

## Deep Dive
Elm on suunniteltu niin, että sivuvaikutukset, kuten tiedoston käsittely tai konsoliin kirjoittaminen, hoidetaan erillisten komentojen kautta. Tavallisesti, kielet kuten C tai Python tukevat standardivirheeseen kirjoittamista suoraan, mutta Elmissä sivuvaikutuksia hallitaan tarkemmin. Tästä johtuen, suora standardivirheeseen kirjoittaminen ei ole idiomaattista Elm-koodia, ja kehittäjät käyttävät `Debug.log` virheiden loggaamiseen kehitysvaiheessa. Tuotantokoodiin tätä ei yleensä jätetä.

## See Also
- Elm virallinen dokumentaatio: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
- `Debug.log` käyttö: [https://package.elm-lang.org/packages/elm/core/latest/Debug#log](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)
- Hyvät käytännöt Elm-koodaukseen: [https://elm-lang.org/docs/style-guide](https://elm-lang.org/docs/style-guide)

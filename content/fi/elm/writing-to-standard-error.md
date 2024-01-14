---
title:                "Elm: Tiedon kirjoittaminen vakiovirheeseen"
simple_title:         "Tiedon kirjoittaminen vakiovirheeseen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisit standardivirheeseen eli standard erroriin? Kuinka voit hyödyntää tätä ohjelmoinnissa?

## Kuinka tehdä

Koodin esimerkkejä ja näiden tulostuksia "```Elm...```" -koodilohkoissa.

````Elm
main : Program ()
main =
    let
        message = "Moi, tämä on standardivirhe!"
    in
    Debug.log "Error" message
    ````

Tämä tulostaa konsoliin "Error: Moi, tämä on standardivirhe!".

## Syvällinen tarkastelu

Standardivirheen kirjoittaminen voi auttaa sinua löytämään ja debuggaamaan ongelmia ohjelmakoodissasi. Se näyttää viestin konsolissa, jossa voit nähdä, missä kohtaa ohjelmaasi virhe ilmenee. Tämä on erityisen hyödyllistä, kun ohjelmasi kasvaa ja sisältää monia osia ja muuttujia.

## Katso myös

- [Elm:n viralliset dokumentaatiot](https://guide.elm-lang.org/)
- [Standardivirheen hallintaa Elm:ssä](https://medium.com/@prasadbobbili/standard-error-handling-in-elm-54680f217833)
- [Elm-kielen virallinen nettisivu](https://elm-lang.org/)
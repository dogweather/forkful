---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Å Slette Tegn Som Matcher et Mønster i Elm

Dette er en nesten lommebokstørrelse guide til å slette tegn som matcher et mønster i Elm. Få koden rett, raskt.

## Hva & Hvorfor?

Sletting av tegn som matcher et mønster gjør det mulig å manipulere og rydde opp i strenger. Programmerere bruker denne teknikken for å redusere rot og uønsket data i kode.

## Hvordan:

Her er en enkel måte å bruke `String.Extra` biblioteket for å slette alle vokaler fra en tekststreng i Elm.

```Elm
import String.Extra

removeVowels : String -> String
removeVowels text = 
    String.Extra.replaceRegex "[aeiouAEIOU]" "" text

-- Eksempel

main =
    let
        text = "Programmering i Elm er gøy!"
        result = removeVowels text
    in
    Html.text result   -- Output: "Prgrmmrng  Elm r gy!"
```

## Dypdykk

Historisk sett, Elm populariserte funksjonell reaktiv programmering på weben. Elm's innebygde `String` bibliotek tilbyr mange funksjoner for strengmanipulasjon, men for å slette tegn som matcher et mønster, må du ty til pakker som `String.Extra`.

Alternativt, du kan implementere din egen løsning ved hjelp av en rekursiv funksjon som itererer over hver karakter i strengen.

I tillegg, sletting av tegn som matcher et mønster kan være ganske effektiv i Elm, siden Elm bruker Unicode under panseret.

## Se Også: 

1. [Elm's String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
2. [Elm's String.Extra Documentation](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
3. [Elm's Regex Documentation](https://package.elm-lang.org/packages/elm/regex/latest/)
4. [Funksjonell programmering i Elm](https://elm-lang.org/)
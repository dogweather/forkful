---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# En Handleiding til Å Finne Lengden på en Streng i Elm

## Hva og Hvorfor?
Å finne lengden på en streng betyr å bestemme antall tegn i en bestemt tekststreng. Dette er viktig for programmerere fordi det lar oss håndtere tekst mer presist og effektivt.

## Hvordan gjør man det:
Her er et eksempel på hvordan man kan finne lengden på en streng i Elm:

```Elm
import String exposing (length)

main =
    let 
        myString = "Hei, Verden!"
    in
        length myString
```

Hvis du kjører dette programmet, vil det gi deg output "12", som er lengden av strengen "Hei, Verden!".

## Dypdykk
Beregnelsen av strenglengden er en grunnleggende operasjon i de fleste programmeringsspråk. I Elm, skjer det gjennom `String.length` funksjonen, som kommer fra String-modulen.
Alternativt kan du bruke en fold til å beregne lengden på en streng hvis du vil unngå å bruke innebygde funksjoner:

```Elm
import List exposing (foldl)

strLen : String -> Int
strLen str =
  List.length <| String.toList str
```
Implementasjonen av `String.length` i Elm er imidlertid mer effektiv enn å bruke List.foldl, siden den er implementert som en enkel iterasjon over tegnene i strengen.

## Se Også
For mer informasjon om strengbehandling i Elm, se følgende kilder:

- Elm sin offisielle dokumentasjon om [String-modulen](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/String)
- For en mer grundig forståelse av strenger, sjekk Elm sin offisielle guide på [Tegn og Strenger](https://guide.elm-lang.org/interop/flags.html) 
- For mer komplekse operasjoner på strenger, se denne artikkelen om [String-manipulasjon i Elm](https://elmprogramming.com/string.html).
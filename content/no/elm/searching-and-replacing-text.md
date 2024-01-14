---
title:    "Elm: Søk og bytt ut tekst"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor
Tekst søking og erstatting er en viktig del av programmering og kan hjelpe deg med å effektivt endre og rette feil i din kode. Med Elm sin robuste og oversiktlige syntaks vil du kunne håndtere dette på en enkel og strukturert måte.

## Hvordan
For å kunne søke og erstatte tekst i Elm trenger vi å bruke funksjonen `replace` som tar inn en karakter eller streng vi ønsker å bytte ut, og en annen karakter eller streng vi vil erstatte det med. La oss se på et eksempel:

```Elm
replace "e" "o" "Hei, verden!" 
```

Eksemplet ovenfor vil gi oss en output på `Hoi, vorlden!` siden funksjonen vil søke etter alle forekomster av karakteren "e" og erstatte de med "o". 

En annen nyttig funksjon i Elm er `replaceOnce` som, som navnet tilsier, kun vil bytte ut den første forekomsten det finner:
```Elm
replaceOnce "k" "t" "katten min"
```

Dette vil gi oss `tatten min` som output. Her ser vi at funksjonen kun har byttet ut den første "k"-en med en "t".

## Dypdykk
Hvis vi ønsker å gjøre mer avanserte søk og erstattinger, kan vi bruke funksjonen `regexReplace` som tar inn et regular expression (regex) i tillegg til strengene vi vil bytte ut. Dette vil åpne opp for flere muligheter for søk og erstatting. La oss se på et eksempel:

```Elm
regexReplace (Regex.parse "(\\d+)" |> Result.withDefault (Regex.fromString ".*"))
    (Tuple.second >> String.reverse >> String.toUpper)
    "A1b2c3d"
```

I eksemplet over vil funksjonen ta alle tallene i strengen og bytte de ut med sin omvendte og storebokstavform, altså `A1b2c3d` vil bli til `AdC2b1A`.

## Se Også
- Slik bruker du regular expressions i Elm: [https://guide.elm-lang.org/interop/regex.html](https://guide.elm-lang.org/interop/regex.html)
- Dokumentasjon for `replace` funksjonen: [https://package.elm-lang.org/packages/elm/core/latest/String#replace](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- Utforsk mer avanserte tekstfunksjoner i Elm: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
---
title:                "Elm: Uthenting av delstrenger"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Å utvinne substrings fra en streng kan være en viktig del av programmering, spesielt når det kommer til behandling av tekstdata. Det lar deg manipulere og arbeide med deler av en streng, i stedet for å måtte gjøre endringer på hele strengen.

# Slik gjør du det

For å utvinne substrings i Elm, kan du bruke funksjonen `String.slice start stop string`. Her er et eksempel:

```Elm
import String

main =
    let
        originalString = "Dette er en eksempelstreng."
        substring = String.slice 0 5 originalString
    in
        substring
```

Dette vil resultere i output "Dette".

# Utforsking av utvinning av substrings

`String.slice` -funksjonen tar tre argumenter: `start`, `stop` og `string`. `Start` er indeksen hvor du vil begynne å utvinne substring fra, mens `stop` er indeksen rett før hvor du vil stoppe utvinning. `String` er selve strengen du vil utvinne substrings fra.

En annen måte du kan utvinne substrings på er ved å bruke `String.dropLeft` eller `String.dropRight` for å fjerne en bestemt del av strengen til venstre eller høyre for en gitt indeks. Du kan også bruke `String.takeLeft` eller `String.takeRight` for å få en del av strengen til venstre eller høyre for en gitt indeks.

Det er også muligheten for å bruke regulære uttrykk for å utvinne substrings fra en streng basert på et mønster.

# Se også

- [Elm's String documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [String functions in Elm](https://elmprogramming.com/functions/string-functions-in-elm.html)
- [Extracting Substrings in Elm](https://medium.com/@joeydeluca13/extracting-substrings-in-elm-e994d3917d04)
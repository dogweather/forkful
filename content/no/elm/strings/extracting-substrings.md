---
aliases:
- /no/elm/extracting-substrings/
date: 2024-01-20 17:45:40.968208-07:00
description: "\xC5 trekke ut delstrenger betyr at vi henter spesifikke deler fra en\
  \ tekststreng. Det gj\xF8r vi for \xE5 bearbeide, analysere eller vise frem informasjon\
  \ p\xE5 en\u2026"
lastmod: 2024-02-18 23:08:53.800536
model: gpt-4-1106-preview
summary: "\xC5 trekke ut delstrenger betyr at vi henter spesifikke deler fra en tekststreng.\
  \ Det gj\xF8r vi for \xE5 bearbeide, analysere eller vise frem informasjon p\xE5\
  \ en\u2026"
title: Uthenting av delstrenger
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å trekke ut delstrenger betyr at vi henter spesifikke deler fra en tekststreng. Det gjør vi for å bearbeide, analysere eller vise frem informasjon på en mer målrettet måte.

## Hvordan:
```Elm
import String exposing (slice)

main =
    let 
        fullString = "Hei, Norge!"
        substring = String.slice 0 3 fullString
    in
    -- Resultat: "Hei"
    substring
```

## Dypdykk
Å trekke ut delstrenger er en vanlig operasjon som strekker seg tilbake til de tidligste programmeringsspråkene. I Elm utfører vi dette med `String.slice`, som lar oss angi start og sluttindeks for delstrengen vi ønsker. En alternativ metode kunne vært å bruke `String.left` eller `String.right` for å få henholdsvis venstre eller høyre del av strengen. Dette kan være mer rett frem hvis vi vet lengden på delstrengen vi er ute etter. Implementasjonsdetaljer inkluderer at indeksene er basert på UTF-16 karakter enheter, noe som kan gi uventede resultater med noen unicode-tegn.

## Se Også
- Elm String documentation: [String functions](https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm language discussion forum: [Elm Discourse](https://discourse.elm-lang.org/)

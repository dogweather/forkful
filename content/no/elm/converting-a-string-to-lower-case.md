---
title:    "Elm: Konvertering av streng til små bokstaver"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være veldig nyttig når du jobber med data og behandler tekst. Dette kan hjelpe deg med å sammenligne og sortere tekst på en mer nøyaktig og konsistent måte.

## Hvordan

Å konvertere en streng til små bokstaver i Elm er en enkel prosess. Du kan bruke funksjonen `String.toLower` og gi den strengen du vil konvertere som argument. La oss se på et eksempel:

```Elm
streng = "ELM PROGRAMMERING"
konvertertStreng = String.toLower streng

-- Resultat:
-- "elm programmering"
```

Som du ser, blir alle bokstavene i strengen omgjort til små bokstaver. Du kan også bruke denne funksjonen på variabler som inneholder strenger.

```Elm
storBokstav = "KODESKOLE"
konvertertStorBokstav = String.toLower storBokstav
```

Dette vil gi samme resultatet som i det forrige eksempelet.

## Dypdykk

Det er viktig å merke seg at `String.toLower` funksjonen bare konverterer ASCII-tegn til små bokstaver. Tegn som æ, ø og å vil ikke bli konvertert. Dette kan føre til uventede resultater hvis du arbeider med tekst som inneholder disse tegnene. For å få en mer pålitelig konvertering, kan du bruke et bibliotek som heter `elm-utf8` som støtter Unicode-tegnkonvertering. Du kan lese mer om dette biblioteket og hvordan du bruker det [her](https://package.elm-lang.org/packages/tiziano88/persistent-vector/latest/).

## Se også

- [Elm dokumentasjon for `String.toLower` funksjonen](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Info om Unicode-tegnkonvertering og `elm-utf8` bibliotek](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List.Extra)
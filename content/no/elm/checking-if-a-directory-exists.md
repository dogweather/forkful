---
title:    "Elm: Sjekker om en mappe eksisterer"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Hvorfor

Å sjekke om en mappe eksisterer kan være viktig når du jobber med filbehandling i Elm. Dette kan bidra til å sikre at koden din fungerer som den skal og unngå eventuelle feil.

## Slik gjør du det

For å sjekke om en mappe eksisterer i Elm, kan du bruke funksjonen `Dir.doesDirectoryExist`, som tar inn en streng som representerer banen til mappen du ønsker å sjekke. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Elm
import Dir exposing (doesDirectoryExist)

checkDirectoryExists : String -> Bool
checkDirectoryExists path =
    doesDirectoryExist path
```

Som du kan se, tar funksjonen `doesDirectoryExist` inn en streng og returnerer en boolsk verdi, som vil være `True` hvis mappen eksisterer og `False` hvis den ikke gjør det.

## Dykk ned i det

Det kan være nyttig å vite at funksjonen `doesDirectoryExist` sjekker både eksistensen og tilgangen til mappen. Det vil si at selv om mappen eksisterer, men du ikke har tilgang til den, vil funksjonen returnere `False`.

Det er også verdt å merke seg at denne funksjonen bare fungerer for mapper, ikke filer. Hvis du vil sjekke om en fil eksisterer, kan du bruke funksjonen `File.isFile`.

## Se også

- [Offisiell dokumentasjon for Dir-modulen](https://package.elm-lang.org/packages/elm/core/latest/Dir)
- [Eksempelkode for å sjekke fil- og mapeksistens i Elm](https://github.com/elm/file/blob/master/tests/FileSystem.elm)
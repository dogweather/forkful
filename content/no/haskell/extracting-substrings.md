---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å trekke ut delstrenger er å selektere spesifikke deler av en streng. Dette brukes når programmerere bare trenger en del av dataen i en større streng.

## Hvordan:

Her er noen grunnleggende eksempler på hvordan man kan trekke ut delstrenger i Haskell:

```Haskell
hoved :: IO ()
hoved = do 
    let streng = "Haskell er gøy"
    putStrLn (take 7 streng) -- "Haskell"
    putStrLn (drop 9 streng) -- "gøy"
    putStrLn (take 3 (drop 9 streng)) -- "gø"
```
Når du kjører koden, vil outputtene være som følger:

```Haskell
Haskell
gøy
gø
```

## Dypdykk:

Historisk sett, bruk av funksjonene 'take' og 'drop' for å trekke ut delstrenger går tilbake til LISP i 1958. Som et funksjonelt språk, legger Haskell vekt på enkelhet og klartekst.

Alternativt kan man bruke en annen tilnærming som for eksempel 'splitAt' funksjonen som returnerer et par av delstrenger.

Hvis du dykker ned i implementeringsdetaljer, vil du se at 'take' og 'drop' opererer på O(n) tid, der n er lengden på strengen.

## Se Også:

For mer informasjon, sjekk ut disse kildene:

- "Learn You a Haskell for Great Good!" av Miran Lipovača, et nyttig innføring i funksjonell programmering med Haskell: http://learnyouahaskell.com/
- Haskell 98 Report, mer teknisk beskrivelse av strengoperasjoner: https://web.archive.org/web/20181224041142/https://www.haskell.org/onlinereport/standard-prelude.html
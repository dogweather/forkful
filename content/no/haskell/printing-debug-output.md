---
title:                "Utskrift av feilsøkingsresultater"
html_title:           "Haskell: Utskrift av feilsøkingsresultater"
simple_title:         "Utskrift av feilsøkingsresultater"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive ut feilsøkningsutdata er en viktig del av programmering. Det hjelper deg med å identifisere og fikse feil i koden din, og sørger for at alt fungerer som det skal.

## Slik gjør du det

Printe ut feilsøkningsutdata i Haskell er enkelt. Du trenger bare å bruke funksjonen "print" og gi den den verdien du vil skrive ut. La oss ta en titt på et enkelt eksempel:

```Haskell
resultat <- lengdeJustering 10 "Hei"
print resultat
```

Dette vil skrive ut verdien av variabelen "resultat", som i dette tilfellet vil være "5", siden "Hei" har en lengde på 3 og vi har angitt en maksimal lengde på 10.

En annen nyttig måte å printe ut feilsøkningsutdata på er å bruke "show" funksjonen. Denne funksjonen konverterer verdien til en streng, som deretter kan printes ut. La oss se på et eksempel:

```Haskell
verdi <- return 5
print $ show verdi
```

Dette vil skrive ut strengen "5" som er konvertert fra verdien vi har gitt.

## Dypdykk

Å printe ut feilsøkningsutdata i Haskell kan gjøres på forskjellige måter, avhengig av hvilken type data du ønsker å printe. Du kan også bruke flere funksjoner som "Debug.Trace.trace" for å printe ut en melding sammen med verdien, og "Debug.Trace.traceShow" for å printe ut en melding og verdien i strengformat.

En viktig ting å huske på er å kun bruke utskrift for feilsøkningsformål, og ikke som en permanent løsning. Du bør alltid slette utskriftsfunksjoner når du er ferdig med å feilsøke og har funnet den riktige løsningen.

## Se også

- [Haskell dokumentasjon](https://www.haskell.org/documentation/)
- [Feilsøkningsverktøy i Haskell](https://wiki.haskell.org/Debugging)
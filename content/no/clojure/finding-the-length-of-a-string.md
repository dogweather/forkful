---
title:    "Clojure: I finne lengden på en streng"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert i Clojure, har du kanskje hatt behov for å finne lengden på en streng (string). Kanskje du jobber med tekstbehandling, dataanalyse eller nettverksapplikasjoner. Uansett formål, er det nyttig å vite hvordan man kan finne lengden på en streng. Det er også en grunnleggende ferdighet i programmering som kan brukes i en rekke ulike situasjoner.

## Slik gjør du det

Først og fremst må du forstå at en streng i Clojure består av en sekvens av tegn (characters). For å finne lengden på en streng, kan du bruke funksjonen `count`. Denne funksjonen tar inn en sekvens som argument og returnerer antall elementer i sekvensen. Her er et eksempel på hvordan du kan bruke `count`-funksjonen på en streng:

```Clojure
(def streng "Hei, verden!")
(count streng)
```

Output:

```Clojure
13
```

Som du kan se, returnerte `count`-funksjonen antall tegn i strengen, inkludert mellomrom og spesialtegn. Det er viktig å merke seg at den returnerer et heltall (integer), ikke en streng. Dette kan være nyttig hvis du trenger å bruke lengden av strengen i andre beregninger.

## Dypdykk

I tillegg til `count`-funksjonen, finnes det også en annen, mer spesifikk funksjon for å finne lengden på en streng. Det er funksjonen `str-length`. Denne funksjonen tar inn en streng som argument og returnerer antall tegn i strengen, akkurat som `count`-funksjonen. Forskjellen er at `str-length` har litt bedre ytelse, spesielt hvis du jobber med veldig lange strenger.

Det er også verdt å nevne at `count`-funksjonen og `str-length`-funksjonen også kan brukes på andre typer sekvenser, som lister, vektorer og kart. Så hvis du trenger å finne lengden på noe annet enn en streng, kan du fortsatt bruke disse funksjonene.

## Se også

- [`count`-funksjonen dokumentasjon](https://clojuredocs.org/clojure.core/count)
- [`str-length`-funksjonen dokumentasjon](https://clojuredocs.org/clojure.string/str-length)
---
title:    "Clojure: Konvertere en streng til små bokstaver"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange situasjoner hvor du trenger å konvertere tekst til små bokstaver, enten det er for å forenkle sammenligninger eller for å gjøre teksten mer lesbart. Uansett hva grunnen er, er det viktig å kunne gjøre dette effektivt og enkelt i Clojure.

## Hvordan gjøre det

```Clojure
;; Definere en streng
(def tekst "NORGES FLOTTESTE BLOGG")

;; Konvertere til små bokstaver
(.toLowerCase tekst)

;; Utskrift: norges flotteste blogg
```

Som vist i eksempelet ovenfor, kan du bruke den innebygde Java-metoden `.toLowerCase` til å konvertere en streng til små bokstaver. Denne metoden tar inn en streng og returnerer en ny streng med bare små bokstaver.

Det er viktig å merke seg at `.toLowerCase` metoden også kan brukes på alle andre typer datastrukturer som implementerer `CharSequence` grensesnittet, for eksempel en vektor med tegn eller en tekstfil.

## Dypdykk

Det er verdt å merke seg at standardfunksjonen for å konvertere tekst til små bokstaver i Clojure er `clojure.string/lower-case`. Denne funksjonen tar også inn en streng som parameter og returnerer en ny streng med små bokstaver. Forskjellen er at denne funksjonen tilbyr mer fleksibilitet, da den også kan håndtere spesielle tilfeller som for eksempel språkspesifikke bokstaver.

En annen ting å huske på er at både `.toLowerCase` og `clojure.string/lower-case` er case-sensitive, noe som betyr at de bare vil konvertere store bokstaver til små bokstaver og ikke påvirke små bokstaver som allerede er en del av strengen.

## Se også

- [Clojure String Functions](https://clojuredocs.org/clojure.string)
- [Java String toLowerCase() Method](https://www.w3schools.com/java/ref_string_tolowercase.asp)
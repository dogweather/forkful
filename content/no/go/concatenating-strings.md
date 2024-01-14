---
title:                "Go: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sette sammen strenger, også kalt concatenation, er en viktig del av å programmere i Go. Ved å kombinere forskjellige strenger, kan du lage mer komplekse tekster som er nyttige for å lage brukervennlige grensesnitt, generere rapporter eller utføre andre operasjoner i programmet ditt.

## Slik gjør du det

Den enkleste måten å sette sammen to strenger på i Go er ved å bruke operatoren "+" mellom dem. La oss se på et enkelt eksempel:

```Go
str1 := "Hei"
str2 := "verden!"

resultat := str1 + str2

fmt.Println(resultat) // Output: Hei verden!
```

Som du ser, blir de to strengene satt sammen til en enkelt streng, og resultatet skrives ut på skjermen. Husk at når du bruker "+", må begge operandene være av typen string. Ellers vil du få en feilmelding.

Du kan også sette sammen flere strenger ved å bruke funksjonen "fmt.Sprint". Dette er spesielt nyttig når du vil kombinere strenger med andre data, for eksempel tall eller variabler. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Go
alder := 30

resultat := fmt.Sprint("Jeg er", alder, "år gammel.")

fmt.Println(resultat) // Output: Jeg er 30 år gammel.
```

En annen måte å sette sammen strenger på er ved å bruke funksjonen "strings.Join". Denne funksjonen tar inn en liste med strenger og en separator som argumenter, og setter sammen strengene med separator mellom dem. La oss se på et eksempel:

```Go
navn := []string{"Marius", "Johansen"}

resultat := strings.Join(navn, " ")

fmt.Println(resultat) // Output: Marius Johansen
```

## Dypdykk

Når du jobber med concatenation i Go, er det viktig å huske på at strenger er uforanderlige, det betyr at de ikke kan endres etter at de er opprettet. Dette betyr at hver gang du bruker en concatenation operasjon, opprettes det en helt ny streng i minnet. Dette kan føre til dårlig ytelse hvis du gjør det mange ganger i løpet av programmet ditt. For å unngå dette, kan du bruke "strings.Builder" for å bygge en streng gradvis uten å opprette nye strenger hver gang.

En annen ting å huske på er at Go bruker UTF-8 som standard for strenger, så hvis du jobber med språk som bruker spesielle tegn, må du være oppmerksom på dette når du setter sammen strenger.

## Se også

- https://gobyexample.com/string-concatenation 
- https://golang.org/pkg/fmt/#Sprint 
- https://golang.org/pkg/strings/#Join 
- https://blog.golang.org/strings
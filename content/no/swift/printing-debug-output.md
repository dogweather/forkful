---
title:                "Utskrift av feilrapporter"
html_title:           "Swift: Utskrift av feilrapporter"
simple_title:         "Utskrift av feilrapporter"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om å skrive ut feilsøkingsresultater? Vel, debugging er en viktig del av utviklingsprosessen og kan hjelpe deg med å identifisere og fikse feil i koden din. Logging ut informasjon om hva som skjer i programmet ditt kan være en verdifull ressurs for å feilsøke og forbedre ytelsen til appen din.

## Hvordan

Det enkleste måten å skrive ut debug output i Swift er ved å bruke funksjonen `print()`. La oss si at vi har en variabel `count` som inneholder antall elementer i en liste. Hvis vi vil skrive ut denne verdien, kan vi gjøre det på følgende måte:

```Swift
print("Antall elementer: \(count)")
```
Dette vil skrive ut "Antall elementer: 10" (hvis for eksempel `count` er 10) i konsollen, noe som kan være nyttig for å følge med på verdiene til variabler mens appen kjører.

Vi kan også legge til ekstra informasjon som kan hjelpe oss med å identifisere hvor i koden utskriften kommer fra, for eksempel:

```Swift
print("Koden i for-løkken er nådd.")
```

Dette vil skrive ut "Koden i for-løkken er nådd." og forteller oss nøyaktig hvilken del av koden som ble kjørt.

## Deep Dive

I tillegg til å bare skrive ut verdier og informasjon, kan vi også bruke `print`-funksjonen til å feilsøke. Ved å legge til `#file`, `#line` og `#function` som argumenter i `print`-funksjonen, kan vi også få vite hvilken fil, linje og funksjon utskriften kommer fra.

```Swift
print("Feil oppstod i filen: \(#file), på linje: \(#line), i funksjonen: \(#function)")
```

Dette kan være spesielt nyttig når du jobber med større og mer komplekse prosjekter.

## See Also

- [Swift Documentation - Printing](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID449)
- [Using print() to debug in Swift](https://www.hackingwithswift.com/read/34/0/using-print-to-debug-in-swift) 
- [Logging and Debugging in Swift](https://www.raywenderlich.com/5009-logging-and-debugging-in-swift)
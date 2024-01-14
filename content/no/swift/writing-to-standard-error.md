---
title:                "Swift: Skriver til standardfeil"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil (standard error) kan være en nyttig teknikk når du jobber med Swift-programmering. Det kan hjelpe deg med å feilsøke og finne problemområder i koden din. Det er også nyttig når du ønsker å logge feilmeldinger til en fil eller til konsollen for senere analyse.

## Hvordan

Det er flere måter å skrive til standardfeil i Swift på, avhengig av hva som passer best for din situasjon. Den enkleste måten er å bruke metoden `print(_:to:)`, som tar to argumenter - en verdi som skal skrives ut og en flyttbar buffer som den skal skrives til. Her er et eksempel på hvordan du kan bruke denne metoden:

```Swift
let errorMessage = "Det har oppstått en feil!"
print(errorMessage, to: &stderr)
```

Dette vil skrive ut feilmeldingen "Det har oppstått en feil!" til standardfeil-buffert (stderr). Du kan også bruke retningstegnet `>>` for å skrive ut direkte til standardoutput-bufferten (stdout):

```Swift
print("Denne meldingen vil skrives til standardfeil!", to: &stderr)
```

Du kan også velge å logge feilmeldinger til en fil ved å først åpne en filstrøm og deretter sende denne som argument til `print(_:to:)` metoden:

```Swift
let file = FileHandle(forWritingAtPath: "errors.txt")!
let errorMessage = "Det har skjedd en uventet feil."
print(errorMessage, to: &file)
```

## Dypdykk

Når du skriver til standardfeil, vil det alltid skilles fra standardoutput ved at meldingene skrives ut i rødt. Dette gjør det enkelt å skille mellom vanlige meldinger og feilmeldinger i konsollen. Det er også viktig å merke seg at skriving til standardfeil kan føre til ytelsesproblemer i større programmer, så det bør bare brukes når det er absolutt nødvendig.

## Se også

- [Swift print(_:to:) metod](https://developer.apple.com/documentation/swift/1541063-print)
- [Feilhåndtering i Swift](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Swift standardbiblioteket](https://developer.apple.com/documentation/swift)
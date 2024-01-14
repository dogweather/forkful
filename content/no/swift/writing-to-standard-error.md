---
title:    "Swift: Skriving til standardfeil"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor
Skriver du programmer i Swift og lurer på hvorfor du skulle bry deg med å skrive til standard error? Les videre for å finne ut hvorfor skriving til standard error er en viktig del av Swift-programmering.

## Hvordan
Skriving til standard error kan være nyttig når du ønsker å varsle om feil, debugging eller andre meldinger som ikke bør vises til brukeren. Du kan enkelt skrive til standard error ved å bruke "print"-funksjonen med parameteren "error" og sende inn en String som inneholder meldingen du ønsker å skrive. 

```Swift
print("Hei, dette er en feilmelding!", to: &stderr)
```

Dette vil skrive meldingen til standard error i stedet for standard output. Du kan også legge til ekstra parametere for å spesifisere endring av format eller separator.

```Swift
print("Summen av 2 og 3 er \(2+3)", to: &stderr, terminator: "\n")
```

Dette vil skrive ut summen til standard error og legge til en linjeskift på slutten.

## Dypdykk
Når du skriver til standard error, vil meldingen din bli sendt direkte til konsollen uten å bli lagt til i standard output. Dette er spesielt nyttig når du kjører din kode i et terminalmiljø og ønsker å skille mellom vanlige meldinger og feilmeldinger. 

I tillegg, når du sender meldinger til standard error, vil de vises med en rød farge i konsollen. Dette gjør det enklere å identifisere og fange opp eventuelle feil som kan oppstå under kjøring av ditt program.

## Se også
For mer informasjon om skriving til standard error i Swift, se følgende ressurser:

- [Swift's Standard Library dokumentasjon](https://developer.apple.com/documentation/swift/1541053-print)
- [Ray Wenderlich's "Working with Standard Error in Swift" tutorial](https://www.raywenderlich.com/6308-working-with-standard-error-in-swift)
- [Stack Overflow discussion on writing to standard error in Swift](https://stackoverflow.com/questions/33102947/writing-to-standard-error-in-swift)
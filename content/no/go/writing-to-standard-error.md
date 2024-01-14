---
title:    "Go: Å skrive til standardfeil"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor skrive til standard error?

Det å skrive til standard error kan være nyttig når man ønsker å skrive ut feilmeldinger eller annen informasjon som ikke bør blandes inn med vanlig programoutput. Det kan bidra til å gjøre debugging og feilsøking enklere.

# Slik gjør du det:

```Go
// Kodeeksempel 1:
fmt.Fprintf(os.Stderr, "Dette er en feilmelding!")
```

```Go
// Kodeeksempel 2:
file, err := os.Open("filnavn.txt")
if err != nil {
    // Skriver ut en tilpasset feilmelding til standard error
    fmt.Fprintf(os.Stderr, "Feil ved å åpne filen: %v\n", err)
}
```

Eksempel 1 viser hvordan man kan skrive ut en enkel melding til standard error ved hjelp av `fmt.Fprintf()`-funksjonen. Eksempel 2 viser hvordan man kan bruke standard error for å håndtere feil i koden.

# Dykk ned i det:

Når man skriver til standard error, blir meldingene skrevet ut til et separat sted i terminalen. Dette gjør det enklere å skille feilmeldinger fra vanlig output, spesielt når man kjører større programmer. Standard error er også nyttig når man ønsker å logge informasjon som ikke skal vises til brukeren.

# Se også:

- [Offisiell dokumentasjon for fmt.Fprintf() (på engelsk)](https://golang.org/pkg/fmt/#Fprintf)
- [Mer om håndtering av feil i Go (på engelsk)](https://blog.golang.org/error-handling-and-go) 
- [En guide til å skrive gode feilmeldinger (på engelsk)](https://blog.gopheracademy.com/advent-2015/solid-go-design-good-fault/)
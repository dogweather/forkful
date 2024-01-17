---
title:                "Sjekk om en mappe eksisterer"
html_title:           "Go: Sjekk om en mappe eksisterer"
simple_title:         "Sjekk om en mappe eksisterer"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekke om en mappe eksisterer er en viktig del av programmering fordi det lar deg håndtere potensielle feil og unngå krasj i applikasjonen din. Det er også nyttig når du trenger å navigere gjennom mapper for å finne og behandle filer.

## Hvordan:
Du kan bruke Go sin innebygde "os" pakke for å sjekke om en mappe eksisterer. Her er et eksempel på hvordan du kan gjøre det:

```Go
import "os"

func main() {
    dir := "./myFolder"

    if _, err := os.Stat(dir); err != nil {
        if os.IsNotExist(err) {
            fmt.Println("Mappen finnes ikke")
        } else {
            fmt.Println("Noe gikk galt")
        }
    } else {
        fmt.Println("Mappen finnes")
    }
}
```

I dette eksempelet bruker vi "os".Stat() -funksjonen for å sjekke om mappen eksisterer. Hvis det er en feil, sjekker vi om den er relatert til at mappen ikke eksisterer. Hvis det er tilfelle, skriver vi ut en melding som indikerer dette. Ellers antar vi at mappen eksisterer og skriver ut en melding som bekrefter dette.

## Dykk ned:
Sjekke om en mappe eksisterer kan være nyttig for å unngå unødvendige feil i applikasjonen din. Det kan også være nyttig å bruke "os" pakken for å utføre andre handlinger på mapper og filer, som å lage eller slette dem.

I eldre versjoner av Go, måtte man bruke "fileutils" pakken for å sjekke om en mappe eksisterer. Men nå er denne funksjonaliteten en del av "os" pakken, noe som gjør det enklere å bruke og mer tilgjengelig.

Når du sjekker om en mappe eksisterer, bruker du egentlig en systemkall til operativsystemet for å utføre denne oppgaven. Hvis du ønsker å dykke dypere ned i det tekniske aspektet av hvordan dette fungerer, kan du lese mer om systemkall og fil operasjoner i Go.

## Se også:
- [Go "os" pakke dokumentasjon](https://golang.org/pkg/os/)
- [Go System Calls tutorial](https://www.geeksforgeeks.org/system-calls-in-golang/)
- [Sammenlikning av systemkall mellom ulike operativsystemer](https://www.in.go/typesof-system-calls-for-file-management-in-linu/ux-file-management-system-calls/)
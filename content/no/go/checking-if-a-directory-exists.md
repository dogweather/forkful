---
title:    "Go: Å sjekke om en mappe eksisterer"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer kan være en viktig del av Go-programmering. Det kan hjelpe deg med å håndtere filbehandling og sikre at programmet ditt fungerer som forventet. I denne bloggartikkelen vil vi dykke inn i hvordan du kan sjekke om en mappe eksisterer i Go.

## Hvordan gjøre det

Sjekking av en mappe eksisterer i Go er enkelt og kan gjøres ved hjelp av standardbiblioteket. Først importerer du "os" pakken og deretter bruker du "Stat" funksjonen for å få informasjon om en fil eller mappe. Her er et eksempel på hvordan du kan implementere dette i Go:

```Go
import (
	"os"
    "log"
)

if _, err := os.Stat("mappe"); os.IsNotExist(err) {
    log.Fatal("Mappen eksisterer ikke.")
}
```

I dette eksempelet ser vi først på filen eller mappen "mappe". Hvis den ikke eksisterer, vil vi få en feilmelding som forteller oss at mappen ikke eksisterer. Hvis mappen eksisterer, vil programmet fortsette å kjøre som normalt.

## Dypdykk

Nå som vi har sett på hvordan du kan implementere sjekking av mapper i Go, kan det være lurt å forstå mer om hva som skjer bak kulissene. "Stat" funksjonen returnerer en "FileInfo" struct som inneholder informasjon om filen eller mappen som blir undersøkt. Hvis denne informasjonen ikke finnes, vil det returneres en feilmelding som forteller oss at filen eller mappen ikke eksisterer. Det er også verdt å merke seg at "Stat" funksjonen også kan brukes til å få informasjon om en fil, ikke bare en mappe.

## Se også

For mer informasjon om "Stat" funksjonen, kan du sjekke ut disse ressursene:

- Offisiell dokumentasjon for "os" pakken: https://golang.org/pkg/os/
- Bloggartikkel om filbehandling i Go: https://blog.golang.org/io2013-talk-subtleties
- YouTube-video om "Stat" funksjonen: https://www.youtube.com/watch?v=oAE7ak4D5EQ

Takk for at du leste denne bloggartikkelen om å sjekke om en mappe eksisterer i Go. Vi håper det var nyttig og at du kan bruke denne kunnskapen i dine kommende prosjekter. Lykke til med programmeringen!
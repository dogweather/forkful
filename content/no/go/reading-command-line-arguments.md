---
title:                "Go: Lesing av kommandolinjeargumenter"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Kommando linje argumenter er en viktig del av Go-programmering, da de lar deg lese og behandle brukerinput i form av argumenter når du kjører et program. Dette kan være nyttig for å tilpasse programmet basert på brukerens behov, eller for å jobbe med store datasett som kan være vanskelige å hardkode. Ved å lære hvordan du leser kommandolinjeargumenter, vil du utvide dine Go-programmeringsevner og få en bedre forståelse av hvordan du kan samhandle med brukeren.

# Hvordan

For å lese kommandolinjeargumenter i Go, kan du bruke standardpakken `flag`. Først må du importere pakken ved å legge til `import "flag"` øverst i filen din.

Deretter må du definere flaggene du vil lese ved hjelp av `flag` pakken. For eksempel, hvis du vil lese en streng som brukeren skriver inn som et argument, kan du bruke følgende kode:

```Go
var inputStr string
flag.StringVar(&inputStr, "input", "", "Tekst som skal behandles")
flag.Parse()
```

Den første linjen definerer en variabel `inputStr` som vil lagre brukerens input. Deretter bruker vi `StringVar` -funksjonen fra `flag` -pakken for å definere et flagg med navnet "input". Dette flagget vil være av typen `string` og vil holde verdien som brukeren angir. Den siste parameteren forklarer hva flagget er for, slik at brukeren vet hvilken input som forventes.

Til slutt bruker vi `flag.Parse()` for å faktisk lese og lagre argumentene som er angitt når programmet kjører.

Du kan da bruke den lagrede variabelen `inputStr` til å behandle brukerens input, for eksempel å skrive den til konsollen eller bruke den til å utføre en funksjon i programmet ditt.

For å teste dette eksemplet kan du kjøre følgende kommando i terminalen:

```bash
go run main.go -input "Hei fra kommandolinjen"
```

Dette vil skrive ut "Hei fra kommandolinjen" som brukerens input til konsollen.

# Dypdykk

I tillegg til å lese enkeltverdide argumenter, kan du også bruke `flag` -pakken til å lese flagger som er satt til å være av type `bool`. For eksempel kan du ha et flagg som sier om brukeren vil inkludere tall i inputen sin eller ikke. Dette kan enkelt gjøres ved å legge til følgende kode:

```Go
var useNumbers bool
flag.BoolVar(&useNumbers, "numbers", false, "Bruk tall i input")
```

Her definerer vi en variabel `useNumbers` av typen `bool` og et flagg med navnet "numbers". Standardverdien er satt til `false` og ved å endre dette til `true` når flagget angis, vil programmet oppføre seg annerledes når det leser brukerens input. For eksempel kan du sjekke om flagget er satt til `true` og deretter konvertere inputen til tall hvis det er nødvendig.

Det er også verdt å merke seg at `flag` -pakken støtter å lese flere argumenter med samme navn, slik at du kan ha flere argumenter med samme inputflagg. Dette kan være nyttig hvis du for eksempel vil lese en liste over tall som brukeren angir.

# Se også

- https://yourbasic.org/golang/command-line-flags/ 
- https://gobyexample.com/command-line-arguments 
- https://golang.org/pkg/flag/
---
title:                "Bruke associative tabeller"
aliases:
- no/go/using-associative-arrays.md
date:                  2024-02-03T18:10:51.250459-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke associative tabeller"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, kjent som maps i Go, lar deg lagre nøkkel-verdi-par der hver unike nøkkel kartlegger til en verdi. Programmerere bruker maps for effektiv datahenting, modifisering og for å vedlikeholde en samling av elementer som kan aksesseres raskt ved hjelp av unike nøkler.

## Hvordan:

Å opprette og initialisere en map i Go kan gjøres på forskjellige måter. Her er et grunnleggende eksempel for å komme i gang:

```go
package main

import "fmt"

func main() {
    // Deklarere og initialisere en map
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // Utdata: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

For å legge til eller oppdatere elementer tilordner du en verdi til en nøkkel slik:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// Utdata: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

Å aksessere en verdi via nøkkelen er enkelt:

```go
fmt.Println("Hex-koden for rød er:", colors["red"])
// Utdata: Hex-koden for rød er: #FF0000
```

For å slette et element, bruk `delete` funksjonen:

```go
delete(colors, "red")
fmt.Println(colors)
// Utdata: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

Iterasjon over en map gjøres ved hjelp av en for-løkke:

```go
for color, hex := range colors {
    fmt.Printf("Nøkkel: %s Verdi: %s\n", color, hex)
}
```

Husk, maps i Go er uordnet. Rekkefølgen for iterasjon er ikke garantert.

## Dypdykk

I Go er maps implementert som hashtabeller. Hver post i tabellen består av to elementer: en nøkkel og en verdi. Nøkkelen hashes for å lagre posten, noe som tillater operasjoner på konstant tid for et lite sett med data og gjennomsnittlig tidskompleksitet på O(1) med riktig hashing, som kan degradere til O(n) i verste fall med mange hashkollisjoner.

Et viktig notat for nye Go-programmerere er at map-typer er referansetyper. Dette betyr at når du sender en map til en funksjon, er eventuelle endringer gjort til mapen innenfor den funksjonen synlige for den som kalte. Dette er forskjellig fra, for eksempel, å sende en struct til en funksjon, der structen kopieres med mindre den sendes ved en peker.

Selv om maps er utrolig allsidige og effektive for de fleste bruksområder som involverer assosiative tabeller, kan det, i ytelseskritiske applikasjoner, være gunstig å bruke datastrukturer med mer forutsigbare ytelsesegenskaper, spesielt hvis nøkkeldistribusjoner kan forårsake hyppige kollisjoner.

Et annet alternativ å vurdere er `sync.Map`, tilgjengelig siden Go 1.9, designet for brukstilfeller hvor nøkler kun skrives en gang, men leses mange ganger, og tilbyr forbedringer i effektivitet i disse scenariene. Imidlertid, for konvensjonelle Go-applikasjoner, er vanlig bruk av map idiomatisk og ofte den anbefalte tilnærmingen for enkelhetens og direkte støttens skyld i språket.

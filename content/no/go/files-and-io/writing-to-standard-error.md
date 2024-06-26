---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:20.598906-07:00
description: "Hvordan: I Go gir `os`-pakken verdien `Stderr`, som representerer filen\
  \ for standardfeil. Du kan bruke den med `fmt.Fprint`, `fmt.Fprintf`, eller\u2026"
lastmod: '2024-03-13T22:44:40.285401-06:00'
model: gpt-4-0125-preview
summary: I Go gir `os`-pakken verdien `Stderr`, som representerer filen for standardfeil.
title: Skrive til standard feil
weight: 25
---

## Hvordan:
I Go gir `os`-pakken verdien `Stderr`, som representerer filen for standardfeil. Du kan bruke den med `fmt.Fprint`, `fmt.Fprintf`, eller `fmt.Fprintln`-funksjonene for å skrive til stderr. Her er et enkelt eksempel:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Skriver en enkel streng til stderr
    _, err := fmt.Fprintln(os.Stderr, "Dette er en feilmelding!")
    if err != nil {
        panic(err)
    }

    // Formattert feilmelding med Fprintf
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "Prosessen ble fullført med %d feil.\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

Eksempelutdata (til stderr):
```
Dette er en feilmelding!
Prosessen ble fullført med 4 feil.
```

Husk, disse meldingene vil ikke vises i den vanlige utdataen (stdout), men i feilstrømmen, som kan omdirigeres separat i de fleste operativsystemer.

## Dypdykk
Konseptet med standardfeil er dypt forankret i Unix-filosofien, som tydelig skiller mellom normal utdata og feilmeldinger for mer effektiv databehandling og håndtering. I Go blir denne konvensjonen omfavnet gjennom `os`-pakken, som gir direkte tilgang til fildeskriptorene for stdin, stdout og stderr.

Selv om det å skrive direkte til `os.Stderr` passer for mange applikasjoner, tilbyr Go også mer sofistikerte loggepakker som `log`, som tilbyr ytterligere funksjoner som tidsstempling og mer fleksible utdatakonfigurasjoner (for eksempel skriving til filer). Å bruke `log`-pakken, spesielt for større applikasjoner eller der mer omfattende loggefunksjoner trengs, kan være et bedre alternativ. Det er også verdt å merke seg at Gos tilnærming til feilhåndtering, som oppmuntrer til å returnere feil fra funksjoner, komplementerer praksisen med å skrive feilmeldinger til stderr, noe som tillater mer finstemt kontroll over feilhåndtering og rapportering.

I bunn og grunn, selv om det å skrive til stderr er en grunnleggende oppgave i mange programmeringsspråk, tilbyr Gos standardbibliotek og designprinsipper både enkle og avanserte veier til håndtering av feilutdata, i tråd med bredere bransjepraksis samtidig som det imøtekommer Gos spesifikke designetos.

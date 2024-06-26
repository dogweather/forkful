---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:01.950141-07:00
description: "Hvordan: I Go h\xE5ndteres feil eksplisitt ved bruk av `error`-typen.\
  \ Funksjoner som kan feile returnerer en feil som sin siste returverdi. \xC5 sjekke\
  \ om denne\u2026"
lastmod: '2024-03-13T22:44:40.275043-06:00'
model: gpt-4-0125-preview
summary: "I Go h\xE5ndteres feil eksplisitt ved bruk av `error`-typen."
title: "H\xE5ndtering av feil"
weight: 16
---

## Hvordan:
I Go håndteres feil eksplisitt ved bruk av `error`-typen. Funksjoner som kan feile returnerer en feil som sin siste returverdi. Å sjekke om denne feilverdien er `nil` vil fortelle deg om en feil har oppstått.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("value must be 100 or less")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Feil:", err)
    } else {
        fmt.Println("Resultat:", result)
    }
    
    // Håndterer en feil på en smidig måte
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Feil:", anotherErr)
    } else {
        fmt.Println("Resultat:", anotherResult)
    }
}
```

Eksempelutdata for koden ovenfor:
```
Feil: value must be 100 or less
Resultat: 100
```

I dette eksempelet returnerer `Compute`-funksjonen enten en beregnet verdi eller en feil. Kalleren håndterer feilen ved å sjekke om `err` ikke er `nil`.

## Dypdykk
Gos tilnærming til feilhåndtering er bevisst rettfram og typesikkert, noe som krever eksplisitte feilsjekker. Dette konseptet står i kontrast til feilhåndtering basert på unntak som man ser i språk som Java og Python, hvor feil blir propalert opp kallstakken med mindre de fanges av en unntakshåndterer. Go-teamet argumenterer med at den eksplisitte håndteringen av feil resulterer i klarere og mer pålitelig kode, da det tvinger programmerere til å adressere feil umiddelbart der de oppstår.

Imidlertid nevner noen kritikere at dette mønsteret kan føre til langdryg kode, spesielt i komplekse funksjoner med mange feilutsatte operasjoner. Som respons har nyere versjoner av Go introdusert mer sofistikerte feilhåndteringsfunksjoner, som feilpakking, noe som gjør det lettere å gi kontekst til en feil uten å miste den opprinnelige feilinformasjonen. Fellesskapet har også sett forslag til nye feilhåndteringsmekanismer, som sjekk/håndter, selv om disse fortsatt er under diskusjon per min siste oppdatering.

Gos filosofi for feilhåndtering legger vekt på forståelse og planlegging for feil som en del av programmets normale flyt. Denne tilnærmingen oppmuntrer til utvikling av mer resiliente og forutsigbare programvarer, selv om det potensielt kan øke i mengden av kode som må skrives. Alternative mønstre og biblioteker finnes for å strømlinjeforme feilhåndtering for spesielt komplekse tilfeller, men Gos innebygde `error`-type forblir fundamentet for feilhåndtering i språket.

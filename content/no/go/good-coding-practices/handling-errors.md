---
title:                "Håndtering av feil"
aliases: - /no/go/handling-errors.md
date:                  2024-02-03T17:58:01.950141-07:00
model:                 gpt-4-0125-preview
simple_title:         "Håndtering av feil"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å håndtere feil i Go innebærer å gjenkjenne og reagere på feilsituasjoner i programmet ditt. Programmere engasjerer seg i feilhåndtering for å sikre at applikasjonene deres kan gjenopprette seg på en kontrollert måte fra uventede situasjoner, noe som fører til mer robust og pålitelig programvare.

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

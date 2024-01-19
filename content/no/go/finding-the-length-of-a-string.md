---
title:                "Finne lengden på en streng"
html_title:           "Go: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng betyr å telle antall karakterer den inneholder. Dette er ofte nyttig i programmeringssituasjoner, som for eksempel når vi ønsker å iterere gjennom hver karakter i strengen, eller når vi trenger å vite størrelsen på dataene vi behandler.

## Slik gjør du:

Her er et enkelt eksempel på hvordan du finner lengden på en streng i Go:

```Go
package main

import "fmt"

func main() {
    str := "Hei på deg, Verden!"
    fmt.Println(len(str))
}
```

Ved kjøretid vil dette skrive ut "19", som er antall karakterer i strengen "Hei på deg, verden!"

## Dypdykk:

Historisk sett har håndtering av strenger alltid vært en sentral del av computing. Fra de tidligste dagene med datainntasting til moderne webapplikasjoner, er strenger, og måling av deres lengde, en nøkkelfunksjon i de fleste programmeringsspråk.

Det finnes alternative måter å finne strenglengden i Go, som å bruke `range`-funksjonen til å iterere gjennom strengen og telle karakterene manuelt. Men `len`-funksjonen er som regel det enkleste og mest effektive alternativet. 

Når det gjelder implementasjonsdetaljer, retter `len`-funksjonen i Go seg mot implementeringen av strengobjektet, noe som betyr at det henter lengden på strengen direkte fra strengstructen. Dette gjør det til en svært rask operasjon sammenlignet med novelleskriving av strengens lengdefunksjon.

## Se Også:

Se detaljert dokumentasjon og mer avansert bruk av `len`-funksjonen i Go:
- Go offisiell dokumentasjon på len - https://golang.org/pkg/builtin/#len
- Go tutorial: strings, bytes, runes and characters in Go - https://blog.golang.org/strings
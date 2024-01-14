---
title:                "Go: Generering av tilfeldige tall"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang hatt behov for å generere tilfeldige tall i dine programmer? Det kan være for å lage unike brukernavn, å lage et spill med tilfeldig lyd eller rett og slett for å teste og simulere forskjellige situasjoner. I denne bloggposten skal vi se nærmere på hvordan du enkelt kan generere tilfeldige tall i Go-programmeringsspråket.

## Hvordan

For å generere tilfeldige tall i Go, kan vi bruke pakken "math/rand". Først må vi importere denne pakken og deretter kan vi bruke funksjonen "rand.Intn(n)" for å generere et tilfeldig tall mellom 0 og n. La oss se på et eksempel:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	randomNumber := rand.Intn(100)
	fmt.Println("Tilfeldig tall:", randomNumber)
}
```

Output:

```
Tilfeldig tall: 27
```

I dette eksemplet brukte vi "rand.Intn(100)" for å generere et tilfeldig tall mellom 0 og 100. Du kan endre 100 til et annet tall for å generere et tilfeldig tall innenfor et annet område.

Hvis du vil generere mer komplekse tilfeldige tall, kan du også bruke funksjonen "rand.Float64()" som vil generere et tilfeldig desimaltall mellom 0.0 og 1.0. Du kan deretter bruke denne funksjonen til å lage et tilfeldig tall innenfor et annet område ved å multiplisere og legge til verdier. For eksempel:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	randomNumber := rand.Float64() * 100 + 50 // Genererer et tilfeldig tall mellom 50 og 150
	fmt.Println("Tilfeldig tall:", randomNumber)
}
```

Output:

```
Tilfeldig tall: 136.57275744643755
```

## Dypdykk

Når du bruker funksjonene i "math/rand" pakken, vil du kanskje legge merke til at de genererte tallene virker å være de samme hver gang du kjører programmet. Dette er fordi Go bruker en standard tallgenerator, som alltid starter med samme startpunkt. For å få virkelig tilfeldige tall, må vi endre dette startpunktet, også kalt "seed".

For å endre seed-en, kan vi bruke funksjonen "rand.Seed(n)" og gi den en forskjellig verdi hver gang vi kjører programmet. Vanligvis bruker man tiden som seed, fordi dette endres hver gang programmet startes. Du kan bruke følgende kode for å endre seed-en:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano()) // Endrer seed-en til datoen og klokkeslettet når programmet starter
	randomNumber := rand.Intn(100)
	fmt.Println("Tilfeldig tall:", randomNumber)
}
```

Output:

```
Tilfeldig tall: 78
```

Nå vil du se at hver gang du kjører programmet, vil det genererte tilfeldige tallet være annerledes.

## Se også

- [Go's pakke for tilfeldige tall](https://golang.org/pkg/math/rand/)
- [Generer tilfeldige tall i Go av Gopher Guides](https://golang.org/doc/play/random.go)
- [Tilfeldighet i Go av Dave Cheney](https://dave.cheney.net/practical-go/presentations/qcon-china.html)
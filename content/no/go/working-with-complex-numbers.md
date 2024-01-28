---
title:                "Å jobbe med komplekse tall"
date:                  2024-01-26T04:41:15.514671-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med komplekse tall"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Komplekse tall, bestående av en reell og en imaginær del (som 5 + 7i), er avgjørende i felt som ingeniørvitenskap, fysikk og signalbehandling. Programmerere jobber med dem for å løse problemer i disse domenene som ville være vanskelige å knekke med bare reelle tall.

## Hvordan:
Go har innebygd støtte for komplekse tall. Her er en kjapp gjennomgang:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Opprette komplekse tall
	a := complex(2, 3)
	b := 4 + 5i

	// Grunnleggende operasjoner
	fmt.Println("Addisjon:", a+b)
	fmt.Println("Subtraksjon:", a-b)
	fmt.Println("Multiplikasjon:", a*b)
	fmt.Println("Divisjon:", a/b)

	// Egenskaper ved komplekse tall
	fmt.Println("Reell del:", real(b))
	fmt.Println("Imaginær del:", imag(b))
	fmt.Println("Konjugat:", cmplx.Conj(b))
	fmt.Println("Størrelse:", cmplx.Abs(b))
	fmt.Println("Fasevinkel (radianer):", cmplx.Phase(b))
}

```

Eksempel på utdata:

```
Addisjon: (6+8i)
Subtraksjon: (-2-2i)
Multiplikasjon: (-7+22i)
Divisjon: (0.5609756097560976+0.0487804878048781i)
Reell del: 4
Imaginær del: 5
Konjugat: (4-5i)
Størrelse: 6.4031242374328485
Fasevinkel (radianer): 0.8960553845713439
```

## Dypdykk
Langt tilbake ble komplekse tall sett på med skepsis – noen mente de var ubrukelige! Over tid ble deres kraft til å beskrive fysiske fenomener klar. De er fundamentale i kvantefysikk, kontrollteori og elektroteknikk, for å nevne noen områder.

I Go representeres komplekse tall ved hjelp av en datatype kalt `complex128` (64 bits for den reelle og imaginære delen hver) eller `complex64` (32 bits hver). Under panseret er disse egentlig bare to `float64`s eller `float32`s limt sammen. Gos standardbibliotek, `math/cmplx`, tilbyr funksjoner for komplekse matteoperasjoner. Dette sparer deg for den vanskelige matematikken og lar deg fokusere på å løse problemer.

Alternativer til Gos innebygde støtte inkluderer å bruke eksterne biblioteker eller å lage din egen håndtering av komplekse tall. Men disse er sjelden nødvendige fordi Gos native støtte er effektiv og godt integrert i språket.

## Se også
Sjekk ut disse lenkene for mer om Gos muligheter med komplekse tall:
- Gos offisielle dokumentasjon: https://golang.org/pkg/math/cmplx/
- En dypere matematikkoppfriskning om komplekse tall: https://www.mathsisfun.com/numbers/complex-numbers.html
- Praktiske anvendelser av komplekse tall i ingeniørvitenskap: https://ieeexplore.ieee.org/document/528dunno

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:58.747167-07:00
description: "Hvordan: I Go h\xE5ndteres komplekse tall ved hjelp av de innebygde\
  \ funksjonene `complex`, `real`, og `imag`, sammen med typene `complex64` og `complex128`\u2026"
lastmod: '2024-03-13T22:44:40.260132-06:00'
model: gpt-4-0125-preview
summary: "I Go h\xE5ndteres komplekse tall ved hjelp av de innebygde funksjonene `complex`,\
  \ `real`, og `imag`, sammen med typene `complex64` og `complex128` (som representerer\
  \ henholdsvis 64-biter og 128-biters komplekse tall)."
title: "\xC5 Arbeide med Komplekse Tall"
weight: 14
---

## Hvordan:
I Go håndteres komplekse tall ved hjelp av de innebygde funksjonene `complex`, `real`, og `imag`, sammen med typene `complex64` og `complex128` (som representerer henholdsvis 64-biter og 128-biters komplekse tall). Her er en rask introduksjon:

```go
package main

import (
	"fmt"
)

func main() {
	// Opprette komplekse tall
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// Aritmetiske operasjoner
	c := a + b
	fmt.Println("Addisjon:", c) // Utdata: Addisjon: (3+2i)

	d := a * b
	fmt.Println("Multiplikasjon:", d) // Utdata: Multiplikasjon: (5+1i)

	// Tilgang til reell og imaginær deler
	reellDel := real(a)
	imagDel := imag(a)
	fmt.Printf("Reell del: %.1f, Imaginær del: %.1f\n", reellDel, imagDel) // Utdata: Reell del: 2.0, Imaginær del: 3.0

	// Kompleks konjugat og størrelse kan beregnes
	konjugat := complex(real(a), -imag(a)) // Manuelt
	fmt.Println("Konjugat av a:", konjugat) // Utdata: Konjugat av a: (2-3i)
}

```

Dette eksemplet dekker grunnleggende informasjon, men det er mye mer du kan gjøre med komplekse tall, inkludert å utnytte pakken `math/cmplx` for mer avanserte operasjoner som å finne størrelse, fase, og mye mer.

## Dypdykk
Konseptet med komplekse tall går tilbake til 1500-tallet, men fikk først bred anerkjennelse og streng formalisering på 1800-tallet. I dataprogrammering har komplekse tall vært en grunnpilar for kompleks aritmetikk i vitenskapelige og ingeniørmessige beregninger siden de tidlige dager. Gos tilnærming til komplekse tall, ved å gjøre dem til førsteklasses borgere med innebygd støtte og omfattende standardbibliotekstøtte gjennom pakken `math/cmplx`, skiller seg ut blant programmeringsspråk. Denne designbeslutningen reflekterer Gos vekt på enkelhet og ytelse.

Det er likevel verdt å merke seg at selv om arbeid med komplekse tall i Go kan være kraftfullt, er det ikke alltid den beste tilnærmingen for alle applikasjoner, særlig de som krever symbolsk matematikk eller høy presisjonsaritmetikk. Språk og miljøer som er spesialiserte på vitenskapelig databehandling, som Python med biblioteker som NumPy og SciPy, eller programvare som MATLAB, kan tilby mer fleksibilitet og et bredere spekter av funksjonaliteter for spesifikke applikasjoner.

Det sagt, for systemprogrammering og kontekster der integrering av komplekse tallberegninger i en større, ytelsesfølsom applikasjon er avgjørende, tilbyr Gos innebygde støtte for komplekse tall et unikt effektivt alternativ.

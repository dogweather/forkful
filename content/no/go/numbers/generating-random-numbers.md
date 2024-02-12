---
title:                "Genererer tilfeldige tall"
aliases: - /no/go/generating-random-numbers.md
date:                  2024-02-03T17:57:24.722175-07:00
model:                 gpt-4-0125-preview
simple_title:         "Genererer tilfeldige tall"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige numre i programmering handler om å skape en sekvens av numre som ikke kan forutses med rimelighet bedre enn ved tilfeldighet. Programmerere gjør dette av en rekke grunner, inkludert simuleringer, spill og sikkerhetsapplikasjoner, der uforutsigbarhet er nøkkelen til funksjonalitet eller hemmelighold.

## Hvordan:

I Go genereres tilfeldige numre ved å bruke `math/rand`-pakken for pseudotilfeldige numre eller `crypto/rand` for kryptografisk sikre pseudotilfeldige numre. La oss utforske begge.

### Bruke `math/rand` for Pseudotilfeldige Numre

Først, importer `math/rand`-pakken og `time`-pakken for å så generatoren. Å så sørger for at du får en annen sekvens av numre ved hvert kjør.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Et tilfeldig nummer:", rand.Intn(100)) // Genererer et nummer mellom 0 og 99
}
```

Eksempel på utdata: `Et tilfeldig nummer: 42`

### Bruke `crypto/rand` for Kryptografisk Sikre Pseudotilfeldige Numre

For applikasjoner med høyere sikkerhetskrav, er `crypto/rand`-pakken egnet siden den genererer tilfeldige numre som er vanskelige å forutsi, noe som gjør dem egnet for kryptografiske operasjoner.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Et sikkert tilfeldig nummer:", n)
}
```

Eksempel på utdata: `Et sikkert tilfeldig nummer: 81`

## Dypdykk

Kjerneforskjellen mellom `math/rand`- og `crypto/rand`-pakkene i Go stammer fra deres kilde til entropi og deres tiltenkte brukstilfeller. `math/rand` genererer pseudotilfeldige numre basert på et innledende frø; dermed er sekvensen deterministisk og kan forutsies hvis frøet er kjent. Dette er passende for scenarier hvor høy ytelse og ikke absolutt uforutsigbarhet er hovedbekymringen, som simuleringer eller spill.

På den andre siden henter `crypto/rand` tilfeldighet fra operativsystemet, noe som gjør det egnet for kryptografiske bruksområder der uforutsigbarhet er avgjørende. Dette kommer imidlertid på bekostning av ytelse og kompleksitet i håndteringen av tallene den genererer (som å håndtere `*big.Int`-typen for heltall).

Historisk har konseptet med generering av tilfeldige numre i datamaskiner alltid danset på grensen til ekte "tilfeldighet," med tidlige systemer som avhengig sterkt av deterministiske algoritmer som etterlignet tilfeldighet. Ettersom datamaskiner utviklet seg, gjorde også disse algoritmene det, og inkluderte mer sofistikerte kilder til entropi fra sine omgivelser.

Til tross for disse fremskrittene, er jakten på perfekt tilfeldighet i databehandling i seg selv paradoksal, gitt datamaskinenes deterministiske natur. Dette er grunnen til at, for de fleste applikasjoner der forutsigbarhet ville vært skadelig, er kryptografisk sikre pseudotilfeldige numre fra kilder som `crypto/rand` det bedre alternativet, til tross for deres overhead.

I essens adresserer Gos tilnærming med to distinkte pakker for generering av tilfeldige numre elegant avveiningene mellom ytelse og sikkerhet, og lar utviklere velge basert på deres spesifikke behov.

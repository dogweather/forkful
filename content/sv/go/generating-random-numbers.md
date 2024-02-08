---
title:                "Generera slumpmässiga nummer"
aliases:
- sv/go/generating-random-numbers.md
date:                  2024-02-03T17:57:33.075012-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generera slumpmässiga nummer"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/generating-random-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga nummer i programmering handlar om att skapa en sekvens av nummer som inte rimligen kan förutsägas bättre än genom slumpen. Programmerare gör detta av en mängd olika anledningar, inklusive simuleringar, spel och säkerhetstillämpningar, där oförutsägbarhet är nyckeln till funktionalitet eller hemlighet.

## Hur man gör:

I Go genereras slumpmässiga nummer med hjälp av paketet `math/rand` för pseudoslumpmässiga nummer eller `crypto/rand` för kryptografiskt säkra pseudoslumpmässiga nummer. Låt oss utforska båda.

### Använda `math/rand` för Pseudoslumpmässiga Nummer

Först, importera paketet `math/rand` och paketet `time` för att seeda generatorn. Seeding säkerställer att du får en annan sekvens av nummer varje körning.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Ett slumpmässigt nummer:", rand.Intn(100)) // Genererar ett nummer mellan 0 och 99
}
```

Exempelutdata: `Ett slumpmässigt nummer: 42`

### Använda `crypto/rand` för Kryptografiskt Säkra Pseudoslumpmässiga Nummer

För mer säkerhetskänsliga applikationer är paketet `crypto/rand` lämpligt eftersom det genererar slumpmässiga nummer som är svåra att förutsäga, vilket gör dem lämpliga för kryptografiska operationer.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Ett säkert slumpmässigt nummer:", n)
}
```

Exempelutdata: `Ett säkert slumpmässigt nummer: 81`

## Djupdykning

Den grundläggande skillnaden mellan paketen `math/rand` och `crypto/rand` i Go kommer från deras källa till entropi och deras avsedda användningsområden. `math/rand` genererar pseudoslumpmässiga nummer baserat på en initial seed; således är sekvensen deterministisk och kan förutsägas om seeden är känd. Detta är lämpligt för scenarier där hög prestanda och inte absolut oförutsägbarhet är huvudbekymret, som simuleringar eller spel.

Å andra sidan hämtar `crypto/rand` slumpmässighet från det underliggande operativsystemet, vilket gör det lämpligt för kryptografiska användningar där oförutsägbarhet är avgörande. Detta kommer dock med kostnader för prestanda och komplexitet i hanteringen av de nummer det genererar (som att hantera `*big.Int`-typen för heltal).

Historiskt sett har begreppet slumpmässig nummergenerering i datorer alltid dansat på gränsen till verklig "slumpmässighet", med tidiga system som i stor utsträckning förlitade sig på deterministiska algoritmer som efterliknade slumpmässighet. I takt med att datorer utvecklades, gjorde även dessa algoritmer det, genom att inkludera mer sofistikerade källor till entropi från deras omgivningar.

Trots dessa framsteg är strävan efter perfekt slumpmässighet i databehandling i grunden paradoxal, med tanke på datorernas deterministiska natur själva. Det är därför, för de flesta tillämpningar där förutsägbarhet skulle vara skadlig, kryptografiskt säkra pseudoslumpmässiga nummer från källor som `crypto/rand` är det bättre alternativet, trots deras overhead.

I grund och botten adresserar Gos tillvägagångssätt med två distinkta paket för generering av slumpmässiga nummer elegant avvägningarna mellan prestanda och säkerhet, vilket gör det möjligt för utvecklare att välja baserat på deras specifika behov.

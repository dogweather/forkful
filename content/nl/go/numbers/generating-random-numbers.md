---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:20.293324-07:00
description: "Het genereren van willekeurige getallen in programmeren gaat over het\
  \ cre\xEBren van een reeks getallen die niet redelijkerwijs beter voorspeld kunnen\
  \ worden\u2026"
lastmod: '2024-03-13T22:44:50.283085-06:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in programmeren gaat over het cre\xEB\
  ren van een reeks getallen die niet redelijkerwijs beter voorspeld kunnen worden\
  \ dan door toeval."
title: Willekeurige getallen genereren
weight: 12
---

## Hoe:
In Go worden willekeurige getallen gegenereerd met het `math/rand` pakket voor pseudo-willekeurige getallen of `crypto/rand` voor cryptografisch veilige pseudo-willekeurige getallen. Laten we beide verkennen.

### Gebruik van `math/rand` voor Pseudo-willekeurige Getallen
Importeer eerst het `math/rand` pakket en het `time` pakket om de generator te zaaien. Zaaien zorgt ervoor dat je elke keer een andere reeks getallen krijgt.

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println("Een willekeurig getal:", rand.Intn(100)) // Genereert een getal tussen 0 en 99
}
```

Voorbeelduitvoer: `Een willekeurig getal: 42`

### Gebruik van `crypto/rand` voor Cryptografisch Veilige Pseudo-willekeurige Getallen
Voor meer beveiligingsgevoelige toepassingen is het `crypto/rand` pakket geschikt omdat het willekeurige getallen genereert die moeilijk te voorspellen zijn, waardoor ze geschikt zijn voor cryptografische bewerkingen.

```go
package main

import (
    "crypto/rand"
    "fmt"
    "math/big"
)

func main() {
    n, _ := rand.Int(rand.Reader, big.NewInt(100))
    fmt.Println("Een veilig willekeurig getal:", n)
}
```

Voorbeelduitvoer: `Een veilig willekeurig getal: 81`

## Diepgaand
Het fundamentele verschil tussen de `math/rand` en `crypto/rand` pakketten in Go komt voort uit hun bron van entropie en hun beoogde gebruiksscenario's. `math/rand` genereert pseudo-willekeurige getallen op basis van een initiële zaad; dus, de reeks is deterministisch en kan voorspeld worden als het zaad bekend is. Dit is geschikt voor scenario's waar hoge prestaties en niet absolute onvoorspelbaarheid de belangrijkste zorg zijn, zoals simulaties of spelletjes.

Aan de andere kant, `crypto/rand` haalt willekeurigheid uit het onderliggende besturingssysteem, waardoor het geschikt is voor cryptografisch gebruik waar onvoorspelbaarheid cruciaal is. Dit gaat echter ten koste van prestaties en complexiteit in de omgang met de getallen die het genereert (zoals omgaan met het `*big.Int` type voor gehele getallen).

Historisch gezien heeft het concept van willekeurige getallengeneratie in computers altijd op de rand van ware "willekeurigheid" gedanst, met vroege systemen die sterk afhankelijk waren van deterministische algoritmes die willekeurigheid nabootsten. Naarmate computers evolueerden, deden deze algoritmes dat ook, waarbij ze geavanceerdere bronnen van entropie uit hun omgevingen opnamen.

Ondanks deze vooruitgang is de zoektocht naar perfecte willekeur in de informatica inherent paradoxaal, gezien de deterministische aard van computers zelf. Dit is waarom, voor de meeste toepassingen waar voorspelbaarheid schadelijk zou zijn, cryptografisch veilige pseudo-willekeurige getallen uit bronnen zoals `crypto/rand` de betere alternatief zijn, ondanks hun overhead.

In essentie adresseert Go's benadering met twee verschillende pakketten voor willekeurige getallengeneratie elegant de afwegingen tussen prestaties en veiligheid, waardoor ontwikkelaars kunnen kiezen op basis van hun specifieke behoeften.

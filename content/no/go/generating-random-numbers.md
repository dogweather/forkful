---
title:                "Generering av tilfeldige tall"
date:                  2024-01-27T20:33:45.268028-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generering av tilfeldige tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall i Go innebærer å bruke `math/rand`-pakken for å produsere pseudo-tilfeldige tall til ulike applikasjoner som å simulere eksperimenter, generere testdata, eller å tilføye uforutsigbarhet til spill. Programmerere utnytter denne funksjonen for å skape dynamiske og mindre forutsigbare programvareoppførsler.

## Hvordan:

For å starte genereringen av tilfeldige tall i Go, må du importere `math/rand`-pakken og `time`-pakken for å så frø til generatoren av tilfeldige tall for mer uforutsigbarhet. Her er et grunnleggende eksempel:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Sår generatoren
	rand.Seed(time.Now().UnixNano())
	
	// Genererer et tilfeldig heltall mellom 0 og 99
	randomInt := rand.Intn(100)
	fmt.Println("Tilfeldig heltall:", randomInt)
	
	// Genererer et tilfeldig flyttall mellom 0.0 og 1.0
	randomFloat := rand.Float64()
	fmt.Println("Tilfeldig flyttall:", randomFloat)
}
```

Eksempel på utdata kan være:

```
Tilfeldig heltall: 42
Tilfeldig flyttall: 0.7304601899194229
```

Husk, hver utførelse produserer ulike tall på grunn av frøingen med den nåværende tiden.

## Dypdykk

`math/rand`-pakken i Go implementerer pseudo-tilfeldige nummergeneratorer (PRNGs) for ulike distribusjoner. Selv om den er ganske effektiv for mange applikasjoner, er det avgjørende å merke seg at tallene generert av `math/rand` ikke er passende for kryptografiske formål på grunn av deres deterministiske natur. For kryptografiske behov er `crypto/rand`-pakken det passende valget, som tilbyr en sikker tilfeldig nummergenerator.

Implementasjonen av `math/rand` er basert på en subtraktiv tilfeldig nummergeneratoralgoritme, som er effektiv og har en relativt lang periode før sekvensene gjentar seg. Imidlertid, for applikasjoner som krever virkelig tilfeldige sekvenser, slik som kryptografiske operasjoner, anbefales maskinvaretilfeldige nummergeneratorer (RNGs) eller `crypto/rand`-pakken, som kobles til systemspesifikke sikre tilfeldighetskilder.

`math/rand` tillater såing for å introdusere variabilitet, men samme frø vil alltid generere samme sekvens av tall, noe som fremhever den deterministiske naturen til dens tilfeldighet. Dette gjør den egnet for simuleringer eller spill hvor reproduserbarhet kan være ønskelig for feilsøking eller testformål.

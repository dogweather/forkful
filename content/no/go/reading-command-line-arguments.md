---
title:    "Go: Leser kommandolinjeargumenter"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Hvorfor

I mange programmeringsprosjekter er det viktig å kunne kommunisere med programmet gjennom kommandolinjen. Dette kan være nyttig for å gi programmet input, kjøre spesifikke funksjoner eller få tilbakemeldinger om hva programmet gjør. I denne bloggposten skal vi se på hvordan man kan lese og håndtere kommandolinjeargumenter i Go-programmeringsspråket. 

# Hvordan gjøre det

For å lese kommandolinjeargumenter i Go, bruker vi **os** pakken og dens **Args** funksjon. Denne funksjonen returnerer en liste over alle argumentene som ble gitt til programmet gjennom kommandolinjen. La oss se på et enkelt eksempel:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	fmt.Println("Argumenter:", args)
}
```

I dette eksempelet bruker vi **os.Args** og skriver ut alle argumentene til terminalen. La oss si at vi kjører programmet med kommandolinjeargumentene *heisann* og *verden*, da vil output være:

```
Argumenter: [programnavn heisann verden]
```

Vi ser at programnavnet er det første elementet i listen, etterfulgt av alle våre argumenter. Dette er viktig å huske, spesielt hvis vi ønsker å hente ut spesifikke argumenter. For eksempel, hvis vi vil ha tak i kun det første argumentet, kan vi bruke **args[1]**. La oss se på et annet eksempel:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	fmt.Println("Første argument:", args[1])
}
```

Hvis vi kjører programmet med kommandolinjeargumentet *hei*, vil output være:

```
Første argument: hei
```

Det er også mulig å bruke **Flag** pakken for å merke spesifikke argumenter. Dette lar oss også spesifisere default verdier og gi et hjelpemelding. For mer informasjon om dette, se gjerne på dokumentasjonen til **Flag** pakken.

# Dypdykk

I tillegg til å lese argumenter fra kommandolinjen, er det også viktig å håndtere dem på en sikker og effektiv måte. Ved å bruke **os.Args** funksjonen, må vi være forsiktige med å håndtere ugyldige argumenter eller argumenter som ikke er spesifisert. Dette kan føre til at programmet vårt krasjer eller fungerer feil. Derfor er det lurt å bruke **if-statements** eller **switches** for å sjekke argumentene på forhånd, og gi riktig tilbakemelding eller prosessere argumentene deretter.

# Se også

- [Dokumentasjon for os.Args pakken](https://golang.org/pkg/os/#Args)
- [Dokumentasjon for Flag pakken](https://golang.org/pkg/flag/)
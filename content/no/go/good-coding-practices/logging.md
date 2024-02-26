---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:06.646208-07:00
description: "Logging i programvareutvikling er prosessen med \xE5 registrere informasjon\
  \ om et programs utf\xF8relse, designet for \xE5 spore dets oppf\xF8rsel og diagnostisere\u2026"
lastmod: '2024-02-25T18:49:38.506127-07:00'
model: gpt-4-0125-preview
summary: "Logging i programvareutvikling er prosessen med \xE5 registrere informasjon\
  \ om et programs utf\xF8relse, designet for \xE5 spore dets oppf\xF8rsel og diagnostisere\u2026"
title: Logging
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Logging i programvareutvikling er prosessen med å registrere informasjon om et programs utførelse, designet for å spore dets oppførsel og diagnostisere problemer. Programmerere implementerer logging for å overvåke programvareytelse, feilsøke feil, og sikre systemets sikkerhet og overholdelse, noe som gjør det til et uunnværlig verktøy for vedlikehold og analyse av applikasjoner.

## Hvordan gjøre det:

I Go kan logging implementeres ved å bruke standardbibliotekpakken `log`. Denne pakken gir enkle loggefunksjoner, som å skrive til standard utdata eller til filer. La oss starte med et grunnleggende eksempel på logging til standard utdata:

```go
package main

import (
	"log"
)

func main() {
	log.Println("Dette er en grunnleggende loggoppføring.")
}
```

Utfall:
```
2009/11/10 23:00:00 Dette er en grunnleggende loggoppføring.
```

Tidsstempelet i begynnelsen av loggoppføringen legges automatisk til av `log`-pakken. Neste, la oss utforske hvordan man logger til en fil i stedet for standard utdata:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("Denne loggoppføringen går til en fil.")
}
```

Nå, la oss implementere et mer avansert brukstilfelle: tilpasse loggformatet. Go lar deg opprette en tilpasset logger med `log.New()`:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "EGEN LOGG: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("Dette er en tilpasset loggmelding.")
}
```

Utfall:
```
EGEN LOGG: 2009/11/10 23:00:00 main.go:11: Dette er en tilpasset loggmelding.
```

Dette eksempelet prefikser hver loggmelding med "EGEN LOGG: " og inkluderer datoen, tiden, og kildefilplasseringen.

## Dypdykk

Go standardbibliotekets `log`-pakke er grei og tilstrekkelig for mange applikasjoner, men den mangler noen av de mer sofistikerte funksjonene som finnes i tredjeparts loggbiblioteker, som strukturert logging, loggrotasjon, og loggnivåbasert logging. Pakker som `zap` og `logrus` tilbyr disse avanserte funksjonene og er høyt ansett i Go-samfunnet for deres ytelse og fleksibilitet.

Strukturert logging, for eksempel, lar deg logge data i et strukturert format (som JSON), noe som er spesielt nyttig for moderne skybaserte applikasjoner der logger kanskje analyseres av ulike verktøy eller tjenester. `zap` er spesielt kjent for sin høye ytelse og lave allokeringsoverhead, noe som gjør den egnet for applikasjoner der hastighet og effektivitet er kritisk.

Historisk sett har logging i Go utviklet seg betydelig siden språkets begynnelse. Tidlige versjoner av Go tilbød de grunnleggende loggefunksjonene som vi ser i `log`-pakken. Imidlertid, ettersom språket vokste i popularitet og kompleksiteten til applikasjonene skrevet i Go økte, begynte samfunnet å utvikle mer sofistikerte loggingbiblioteker for å møte deres behov. I dag, mens standard `log`-pakken fortsatt er et levedyktig alternativ for enkle applikasjoner, vender mange utviklere seg til disse tredjepartsløsningene for mer komplekse loggekrav.
